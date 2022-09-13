"""
This program converts `descr_model_battle.txt` (DMB) from
_Rome: Total War_ (OG) to _Total War: Rome Remastered_ (RR).

OG2RR DMB Converter v0.2.0
Copyright 2022 Vartan Haghverdi
"""


import os, sys, base64
import PySimpleGUI as sg
from dmb import convert_dmb_og2rr


def resource_path(relative_path):
    """Get absolute path to resource; helper function for PyInstaller"""
    try:
        base_path = sys._MEIPASS
    except Exception:
        base_path = os.path.abspath(".")
    return os.path.join(base_path, relative_path)


# window setup
sg.theme("darkamber")

font_title = ("Helvetica", 18)
font_btn = ("Helvetica", 12)

title = "OG2RR DMB Converter"

about_text = """OG2RR DMB Converter v0.2.0
Brought to you by the EB Online Team
Copyright 2022 Vartan Haghverdi"""

instructions = """1. Select OG DMB.
2. Select save location.
3. Convert DMB."""

file_types = (("Text files (.txt)", "*.txt"), ("All files", "*.* *"))

progress = sg.ProgressBar(0, visible=False, expand_x=True, size=(1, 1))

layout = [
    [sg.Push(), sg.B("About")],
    [sg.Push(), sg.T(title, font=font_title), sg.Push()],
    [sg.Push(), sg.T(instructions), sg.Push()],
    [
        sg.T("OG DMB", s=8),
        sg.Input(),
        sg.FileBrowse(s=8, file_types=file_types),
    ],
    [
        sg.T("RR DMB", s=8),
        sg.Input(),
        sg.FileSaveAs(s=8, file_types=file_types),
    ],
    [sg.Push(), sg.B("Convert DMB", s=(12, 2), font=font_btn), sg.Push()],
    [progress],
]


# load icon and initialize window
icon_path = resource_path("icon.png")
with open(icon_path, "rb") as f:
    icon = base64.b64encode(f.read())
    window = sg.Window(title, layout, icon=icon)


# main event loop
while True:
    # fetch input values
    event, values = window.read()
    dmb_og_path = values[0] if values else None
    dmb_rr_path = values[1] if values else None

    # break loop if window is closed
    if event == sg.WIN_CLOSED:
        break

    # user clicks "Convert DMB" button
    if event == "Convert DMB":
        # alert user if either OG or RR DMB paths are missing
        if not dmb_og_path:
            sg.popup_error("Must select OG DMB file.", title="Error", icon=icon)
            continue
        if not dmb_rr_path:
            sg.popup_error("Must select RR DMB file.", title="Error", icon=icon)
            continue

        # convert DMB; popup for either success or error
        try:
            model_skip_count = convert_dmb_og2rr(
                dmb_og_path, dmb_rr_path, progress.update
            )
            sg.popup("Converted OG DMB to RR.", title="Success", icon=icon)
            if model_skip_count:
                sg.popup_error(
                    f"Skipped {model_skip_count} models. See `og2rr_error.txt` for more information.",
                    title="Error",
                    icon=icon,
                )
        except Exception as err:
            print(err)
            sg.popup_error(err.args[0], title="Error", icon=icon)

    if event == "About":
        sg.popup(about_text, title="About", icon=icon)


# end app
window.close()
