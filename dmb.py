import re, json
from os.path import isfile


# regex patterns
# re_type = re.compile(r"type\s([^;\n]+)")
re_type = re.compile(r"^\s*type\s([^;\n]+)", re.M)
# re_skeleton = re.compile(r"skeleton\s([^;\n]+)")
re_skeleton = re.compile(r"^\s*skeleton\s([^;\n]+)", re.M)
# re_skeleton_horse = re.compile(r"skeleton_horse([^;\n]+)")
re_skeleton_horse = re.compile(r"^\s*skeleton_horse([^;\n]+)", re.M)
# re_skeleton_elephant = re.compile(r"skeleton_elephant([^;\n]+)")
re_skeleton_elephant = re.compile(r"^\s*skeleton_elephant([^;\n]+)", re.M)
# re_skeleton_chariot = re.compile(r"skeleton_chariot([^;\n]+)")
re_skeleton_chariot = re.compile(r"^\s*skeleton_chariot([^;\n]+)", re.M)
# re_skeleton_camel = re.compile(r"skeleton_camel([^;\n]+)")
re_skeleton_camel = re.compile(r"^\s*skeleton_camel([^;\n]+)", re.M)
# re_texture = re.compile(r"texture\s(.+,)?.+(data[^;\n]+)")
re_texture = re.compile(r"^\s*texture\s(.+,)?.+(data[^;\n]+)", re.M)
# re_model = re.compile(r"model_flexi(?:_m|_c)?.+(data.+),.+")
re_model = re.compile(r"^\s*model_flexi(?:_m|_c)?.+(data.+),.+", re.M)


def convert_dmb_og2rr(dmb_og_path, dmb_rr_path, progress):
    """Convert OG DMB to RR. Only keep the first model (assume `model_flexi`). Assign generic PBR texture."""

    # list of `type` names skipped due to missing textures
    skipped_types = []

    # output text file containing any skipped `type` names
    error_filepath = "og2rr_error.txt"

    # validate OG DMB path
    if not isfile(dmb_og_path):
        raise FileNotFoundError("Invalid DMB file.")

    # read OG DMB and find all entries
    with open(dmb_og_path) as f:
        dmb = f.read()
        re_entry = re.compile(r"type(?:.+\n)+(?:model_sprite|model_tri).+")
        matches = re_entry.findall(dmb)

    # parse each entry into a list of dictionaries
    entries = []
    for idx, match in enumerate(matches):
        # update GUI progress bar
        progress(idx, len(matches), visible=True)

        # empty dictionary to hold entry data
        entry = dict()

        type = re_type.search(match).group(1).strip()
        if not type:
            raise Exception("Could not 'type' parameter of a model.")
        entry["type"] = type

        skeleton = re_skeleton.search(match).group(1).split(",")
        skeleton = list(map(lambda x: x.strip(), skeleton))
        if not len(skeleton):
            raise Exception(f"Could not find skeleton for {type}.")
        entry["skeleton"] = skeleton

        # optional
        skeleton_horse = re_skeleton_horse.search(match)
        if skeleton_horse:
            skeleton_horse = skeleton_horse.group(1).strip()
            entry["skeleton_horse"] = skeleton_horse

        # optional
        skeleton_elephant = re_skeleton_elephant.search(match)
        if skeleton_elephant:
            skeleton_elephant = skeleton_elephant.group(1).strip()
            entry["skeleton_elephant"] = skeleton_elephant

        # optional
        skeleton_chariot = re_skeleton_chariot.search(match)
        if skeleton_chariot:
            skeleton_chariot = skeleton_chariot.group(1).strip()
            entry["skeleton_chariot"] = skeleton_chariot

        # optional
        skeleton_camel = re_skeleton_camel.search(match)
        if skeleton_camel:
            skeleton_camel = skeleton_camel.group(1).strip()
            entry["skeleton_camel"] = skeleton_camel

        textures = []
        for texture in re_texture.findall(match):
            # faction optional
            faction, texture_path = texture
            if faction:
                textures.append(f"{faction.strip()[:-1]}, {texture_path.strip()}")
            else:
                textures.append(texture_path.strip())
        if not len(textures):
            # raise Exception(f"Could not find textures for {type}.")
            skipped_types.append(type)
            continue
        entry["texture"] = textures

        model = re_model.search(match).group(1).strip()
        if not model:
            raise Exception(f"Could not find model for {type}.")
        entry["model"] = model

        entries.append(entry)

    # write RR DMB
    with open(dmb_rr_path, mode="w") as f:
        generic_pbr = "data/characters/textures/generic_pbr.tga"

        for idx, entry in enumerate(entries):
            # update GUI progress bar
            progress(idx, len(entries))

            f.write(f"type                        {entry['type']}\n")
            f.write(f"skeleton                    {entry['skeleton'][0]}")
            if len(entry["skeleton"]) == 2:
                f.write(f", {entry['skeleton'][1]}\n")
            else:
                f.write("\n")
            if "skeleton_horse" in entry:
                f.write(f"skeleton_horse              {entry['skeleton_horse']}\n")
            if "skeleton_elephant" in entry:
                f.write(f"skeleton_elephant           {entry['skeleton_elephant']}\n")
            if "skeleton_chariot" in entry:
                f.write(f"skeleton_chariot            {entry['skeleton_chariot']}\n")
            if "skeleton_camel" in entry:
                f.write(f"skeleton_camel              {entry['skeleton_camel']}\n")
            f.write(f"pbr_texture                 {generic_pbr}\n")
            for texture in entry["texture"]:
                f.write(f"texture                     {texture}\n")
            f.write(f"model_flexi                 {entry['model']}\n")
            f.write(f"no_variation model_flexi    {entry['model']}\n\n")

    # if any models lacked textures, list them in error file and return skip count
    if len(skipped_types):
        with open(error_filepath, mode="w") as f:
            f.write(f"OG DMB: {dmb_og_path}\n\n")
            f.write(
                f"{len(skipped_types)} `type` names were skipped due to missing `texture` lines:\n\n"
            )
            for type in skipped_types:
                f.write(f"{type}\n")

        return len(skipped_types)

    # no skipped models
    else:
        return 0
