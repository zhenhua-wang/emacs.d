cd ~
# yay
sudo pacman -S --needed git base-devel cmake inetutils
git clone https://aur.archlinux.org/yay.git
cd ~/yay && makepkg -si && cd ~

# fonts
yay -S noto-fonts noto-fonts-cjk noto-fonts-emoji

# emacs
mv ~/emacs.d ~/.emacs.d
yay -S emacs-nativecomp emacs-lsp-booster aspell aspell-en
emacs -Q --batch --eval "(require 'org)" --eval '(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))' --eval '(defun zw/org-babel-tangle-linux (path) (if (eq system-type (intern "gnu/linux")) path "no"))' --eval '(defun zw/org-babel-tangle-not-exist (path) (if (file-exists-p path) "no" path))' --eval '(org-babel-tangle-file "~/.emacs.d/resources/OrgFiles/dotfiles.org")'

# gnome core
yay -S gdm gnome-shell gnome-control-center gnome-keyring gnome-tweaks networkmanager xdg-desktop-portal-gnome xdg-user-dirs gst-plugins-good power-profiles-daemon switcheroo-control flatpak malcontent iio-sensor-proxy networkmanager-openconnect gnome-remote-desktop gnome-firmware gnome-connections
sudo systemctl enable gdm.service -f
# gnome extra
yay -S gnome-themes-extra gnome-browser-connector gnome-shell-extension-appindicator gnome-shell-extension-caffeine gnome-shell-extension-vitals gnome-shell-extension-dash-to-panel ibus-rime rime-ice-git nautilus gnome-clocks gnome-calculator gnome-disk-utility gnome-system-monitor baobab loupe

# xremap
yay -S xremap-x11-bin
sudo gpasswd -a $USER input
echo 'KERNEL=="uinput", GROUP="input", TAG+="uaccess"' | sudo tee /etc/udev/rules.d/99-input.rules
######## if not working on gnome wayland, try 'xremap-gnome-bin' ########
# yay -S xremap-gnome-bin
# if [ ! -d ~/.local/share/gnome-shell/extensions/ ]; then
#     mkdir ~/.local/share/gnome-shell/extensions/
# fi
# git clone https://github.com/xremap/xremap-gnome ~/.local/share/gnome-shell/extensions/xremap@k0kubun.com
######## end ########

# bluetooth
sudo systemctl enable bluetooth.service

# research
yay -S pandoc-cli texlive-basic texlive-latexextra texlive-fontsrecommended texlive-binextra texlive-mathscience texlive-plaingeneric texlive-bibtexextra miniconda3

# zsh
yay -S zsh zsh-syntax-highlighting zsh-autosuggestions && chsh -s $(which zsh)
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
echo -e 'source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh' >> ~/.zshrc
echo -e 'source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh' >> ~/.zshrc
echo -e "bindkey '^H' backward-kill-word" >> ~/.zshrc
echo -e 'alias streamlink="streamlink --player mpv"' >> ~/.zshrc
echo -e 'alias open="xdg-open"' >> ~/.zshrc

# config git
git config --global user.name "Zhenhua Wang"
git config --global user.email "wang_zhenhua1996@icloud.com"
git config --global credential.helper store

# essential desktop
yay -S firefox kitty fzf htop fastfetch ripgrep mpv yt-dlp streamlink file-roller exfat-utils

# flatpak applications
flatpak install com.github.tchx84.Flatseal com.github.johnfactotum.Foliate de.haeckerfelix.Fragments io.gitlab.adhami3310.Impression app.drey.Warp com.github.finefindus.eyedropper io.github.mrvladus.List com.obsproject.Studio org.gnome.Showtime org.gnome.Evince org.gimp.GIMP org.inkscape.Inkscape org.libreoffice.LibreOffice us.zoom.Zoom
sudo flatpak override --filesystem=$HOME/.themes
sudo flatpak override --filesystem=$HOME/.icons

# xorg for compatibility
yay -S xorg-server

# config
bash ~/.emacs.d/resources/scripts/zw-gnome-config.sh
bash ~/.emacs.d/resources/scripts/zw-gnome-keymap.sh
sudo ln -s /usr/bin/gnome-session-quit /usr/bin/logout-gnome
