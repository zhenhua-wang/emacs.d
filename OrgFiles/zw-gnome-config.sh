gsettings set org.gnome.desktop.interface gtk-theme "Adwaita"
gsettings set org.gnome.desktop.interface cursor-theme "Breeze"
gsettings set org.gnome.desktop.interface clock-show-weekday true
gsettings set org.gnome.desktop.interface enable-hot-corners false
gsettings set org.gnome.desktop.privacy remember-recent-files false
gsettings set org.gnome.system.location enabled true
gsettings set org.gnome.desktop.datetime automatic-timezone true
# window
gsettings set org.gnome.desktop.wm.preferences mouse-button-modifier "<Super>"
gsettings set org.gnome.desktop.wm.preferences resize-with-right-button true
gsettings set org.gnome.mutter attach-modal-dialogs false
# touchpad
gsettings set org.gnome.desktop.peripherals.touchpad natural-scroll false
gsettings set org.gnome.desktop.peripherals.touchpad tap-to-click true
gsettings set org.gnome.desktop.peripherals.touchpad tap-and-drag false
# input
gsettings set org.gnome.desktop.input-sources sources "[('ibus', 'rime'), ('xkb', 'us')]"
# extension
gsettings set org.gnome.shell disable-user-extensions false
gnome-extensions enable blur-my-shell@aunetx
gnome-extensions enable dash-to-dock@micxgx.gmail.com
gnome-extensions enable Vitals@CoreCoding.com
gnome-extensions enable appindicatorsupport@rgcjonas.gmail.com
gnome-extensions enable hide-universal-access@akiirui.github.io
gnome-extensions enable caffeine@patapon.info
# dash to dock
gsettings set org.gnome.shell.extensions.dash-to-dock dock-position "RIGHT"
gsettings set org.gnome.shell.extensions.dash-to-dock show-trash false
gsettings set org.gnome.shell favorite-apps "['org.gnome.Nautilus.desktop', 'firefox.desktop', 'org.gnome.DiskUtility.desktop', 'io.github.celluloid_player.Celluloid.desktop', 'org.gnome.baobab.desktop', 'com.obsproject.Studio.desktop', 'app.drey.Warp.desktop', 'io.gitlab.adhami3310.Impression.desktop', 'com.github.finefindus.eyedropper.desktop', 'com.github.johnfactotum.Foliate.desktop', 'de.haeckerfelix.Fragments.desktop']"
# blur my shell
gsettings set org.gnome.shell.extensions.blur-my-shell.panel customize true
gsettings set org.gnome.shell.extensions.blur-my-shell.panel static-blur false
gsettings set org.gnome.shell.extensions.blur-my-shell.panel brightness 0.15
# vitals
gsettings set org.gnome.shell.extensions.vitals hot-sensors "['__network-rx_max__', '__temperature_max__']"
gsettings set org.gnome.shell.extensions.vitals hide-icons true
gsettings set org.gnome.shell.extensions.vitals fixed-widths true
# caffeine
gsettings set org.gnome.shell.extensions.caffeine restore-state true
# default app
gio mime inode/directory org.gnome.Nautilus.desktop
