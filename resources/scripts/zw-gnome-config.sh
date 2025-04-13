gsettings set org.gnome.desktop.interface gtk-theme "Adwaita"
gsettings set org.gnome.desktop.interface clock-show-weekday true
gsettings set org.gnome.desktop.interface enable-hot-corners false
gsettings set org.gnome.desktop.privacy remember-recent-files false
gsettings set org.gnome.system.location enabled true
gsettings set org.gnome.desktop.datetime automatic-timezone true
gsettings set org.gnome.shell favorite-apps "['org.gnome.Nautilus.desktop', 'org.gnome.DiskUtility.desktop', 'org.gnome.Software.desktop', 'net.nokyan.Resources.desktop', 'org.gnome.baobab.desktop', 'app.drey.Warp.desktop', 'io.gitlab.adhami3310.Impression.desktop', 'com.obsproject.Studio.desktop', 'de.haeckerfelix.Fragments.desktop', 'com.github.johnfactotum.Foliate.desktop', 'com.github.finefindus.eyedropper.desktop', 'org.gimp.GIMP.desktop']"
# window
gsettings set org.gnome.desktop.wm.preferences mouse-button-modifier "<Super>"
gsettings set org.gnome.desktop.wm.preferences resize-with-right-button true
gsettings set org.gnome.mutter attach-modal-dialogs false
# touchpad
gsettings set org.gnome.desktop.peripherals.touchpad natural-scroll false
gsettings set org.gnome.desktop.peripherals.touchpad tap-to-click true
gsettings set org.gnome.desktop.peripherals.touchpad tap-and-drag false
# input
gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'us'), ('ibus', 'rime')]"
gsettings set org.gnome.desktop.input-sources per-window true
# extension
gsettings set org.gnome.shell disable-user-extensions false
gnome-extensions enable Vitals@CoreCoding.com
gnome-extensions enable caffeine@patapon.info
gnome-extensions enable weatheroclock@CleoMenezesJr.github.io
# vitals
gsettings set org.gnome.shell.extensions.vitals hot-sensors "['__temperature_max__', '_memory_usage_']"
gsettings set org.gnome.shell.extensions.vitals hide-icons true
gsettings set org.gnome.shell.extensions.vitals fixed-widths true
# caffeine
gsettings set org.gnome.shell.extensions.caffeine restore-state true
# weather
gsettings set org.gnome.GWeather4 temperature-unit 'centigrade'
# default app
gio mime inode/directory org.gnome.Nautilus.desktop
xdg-settings set default-web-browser org.mozilla.firefox.desktop
xdg-mime default emacs-Q.desktop text/plain
xdg-mime default emacs-Q.desktop text/x-tex
# ibus
dconf write /desktop/ibus/general/hotkey/triggers "['<Ctrl>backslash', '<Alt>space']"
dconf write /desktop/ibus/general/preload-engines "['xkb:us::eng', 'rime']"
dconf write /desktop/ibus/general/use-global-engine false
dconf write /desktop/ibus/panel/use-custom-theme true
dconf write /desktop/ibus/panel/use-custom-font true
dconf write /desktop/ibus/panel/custom-font "'Sans 13'"
