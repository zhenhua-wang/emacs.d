gsettings set org.gnome.desktop.interface gtk-theme "Adwaita"
gsettings set org.gnome.desktop.interface clock-show-weekday true
gsettings set org.gnome.desktop.interface enable-hot-corners false
gsettings set org.gnome.desktop.privacy remember-recent-files false
gsettings set org.gnome.system.location enabled true
gsettings set org.gnome.desktop.datetime automatic-timezone true
gsettings set org.gnome.shell favorite-apps "['org.gnome.Nautilus.desktop', 'firefox.desktop', 'org.gnome.DiskUtility.desktop', 'io.github.celluloid_player.Celluloid.desktop', 'org.gnome.baobab.desktop', 'com.obsproject.Studio.desktop', 'app.drey.Warp.desktop', 'io.gitlab.adhami3310.Impression.desktop', 'com.github.finefindus.eyedropper.desktop', 'io.github.mrvladus.List.desktop', 'com.github.johnfactotum.Foliate.desktop', 'de.haeckerfelix.Fragments.desktop']"
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
gnome-extensions enable appindicatorsupport@rgcjonas.gmail.com
gnome-extensions enable caffeine@patapon.info
# vitals
gsettings set org.gnome.shell.extensions.vitals hot-sensors "['__network-rx_max__', '__temperature_max__']"
gsettings set org.gnome.shell.extensions.vitals hide-icons true
gsettings set org.gnome.shell.extensions.vitals fixed-widths true
# caffeine
gsettings set org.gnome.shell.extensions.caffeine restore-state true
# default app
gio mime inode/directory org.gnome.Nautilus.desktop
