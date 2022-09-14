import re
from os.path import isfile


# regex patterns
re_entry = re.compile(r"^type(?:.+\n)+(?:model_sprite|model_tri).+", re.M)
re_type = re.compile(r"^type\s([^;\n]+)", re.M)
re_skeleton = re.compile(r"^skeleton\s([^;\n]+)", re.M)
re_skeleton_horse = re.compile(r"^skeleton_horse([^;\n]+)", re.M)
re_skeleton_elephant = re.compile(r"^skeleton_elephant([^;\n]+)", re.M)
re_skeleton_chariot = re.compile(r"^skeleton_chariot([^;\n]+)", re.M)
re_skeleton_camel = re.compile(r"^skeleton_camel([^;\n]+)", re.M)
re_texture = re.compile(r"^texture\s(.+,)?.+(data[^;\n]+)", re.M)
re_model = re.compile(r"^model_flexi(_m|_c)?.+(data.+),([^;\n]+)", re.M)


def format_dmb(dmb: str) -> str:
    # remove initial and trailing whitespace
    dmb = dmb.splitlines()
    dmb = [line.strip() for line in dmb]

    # remove empty lines
    dmb = "\n".join(filter(None, dmb))

    # remove comment lines
    dmb = re.sub(r"^\s*;.*\s*", "", dmb, flags=re.M)

    # separate entries with an empty line
    dmb = re.sub(r"^type", "\ntype", dmb, flags=re.M)
    dmb = "\n".join(dmb.splitlines()[1:] + [""])

    return dmb


def convert_dmb_og2rr(dmb_og_path, dmb_rr_path, log_filepath, progress):
    """Convert OG DMB to RR. Assign generic PBR texture."""

    # list of entries with missing textures
    skipped_types = []

    # validate OG DMB path
    if not isfile(dmb_og_path):
        raise FileNotFoundError("Invalid DMB file.")

    # read OG DMB and find all entries
    with open(dmb_og_path) as f:
        dmb = f.read()
        dmb = format_dmb(dmb)
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
            skipped_types.append(type)
        entry["texture"] = textures

        models = re_model.findall(match)
        if not models:
            raise Exception(f"Could not find model for {type}.")
        entry["models"] = []
        for model in models:
            # (suffix, path, distance)
            model = tuple(map(lambda x: x.strip(), model))
            entry["models"].append(model)

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
            for model in entry["models"]:
                suffix, path, distance = model
                if suffix:
                    f.write(f"model_flexi{suffix}               {path}, {distance}\n")
                else:
                    f.write(f"model_flexi                 {path}, {distance}\n")
            for model in entry["models"]:
                suffix, path, distance = model
                if suffix:
                    f.write(f"no_variation model_flexi{suffix}  {path}, {distance}\n")
                else:
                    f.write(f"no_variation model_flexi    {path}, {distance}\n")
            f.write("\n")

    # if any models lacked textures, list them in error file and return skip count
    if len(skipped_types):
        with open(log_filepath, mode="w") as f:
            f.write(f"OG DMB: {dmb_og_path}\n\n")
            f.write(f"{len(skipped_types)} entries are missing textures:\n\n")
            for type in skipped_types:
                f.write(f"{type}\n")
        return len(skipped_types)

    # no skipped models
    return 0
