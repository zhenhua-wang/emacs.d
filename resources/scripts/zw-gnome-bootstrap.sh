cd ~
# yay
sudo pacman -S --needed git base-devel cmake inetutils
git clone https://aur.archlinux.org/yay.git
cd ~/yay && makepkg -si && cd ~

# fonts
yay -S noto-fonts noto-fonts-cjk noto-fonts-emoji

# emacs
mv ~/emacs.d ~/.emacs.d
yay -S emacs emacs-lsp-booster aspell aspell-en
emacs -Q --batch --eval "(require 'org)" --eval '(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))' --eval '(defun zw/org-babel-tangle-linux (path) (if (eq system-type (intern "gnu/linux")) path "no"))' --eval '(defun zw/org-babel-tangle-not-exist (path) (if (file-exists-p path) "no" path))' --eval '(org-babel-tangle-file "~/.emacs.d/resources/OrgFiles/dotfiles.org")'

# gnome core
yay -S gdm gnome-shell gnome-control-center gnome-keyring gnome-tweaks networkmanager xdg-desktop-portal-gnome xdg-user-dirs gst-plugins-good power-profiles-daemon switcheroo-control flatpak malcontent iio-sensor-proxy networkmanager-openconnect gnome-remote-desktop gnome-firmware gnome-connections
sudo systemctl enable gdm.service -f
# gnome extra
yay -S gnome-themes-extra gnome-browser-connector gnome-shell-extension-appindicator gnome-shell-extension-caffeine gnome-shell-extension-vitals gnome-shell-extension-dash-to-panel ibus-rime rime-ice-git nautilus gnome-clocks gnome-calculator gnome-disk-utility gnome-system-monitor baobab loupe

# xremap for gnome wayland
yay -S xremap-gnome-bin
sudo gpasswd -a $USER input
echo 'KERNEL=="uinput", GROUP="input", TAG+="uaccess"' | sudo tee /etc/udev/rules.d/99-input.rules
if [ ! -d ~/.local/share/gnome-shell/extensions/ ]; then
    mkdir ~/.local/share/gnome-shell/extensions/
fi
git clone https://github.com/xremap/xremap-gnome ~/.local/share/gnome-shell/extensions/xremap@k0kubun.com

# bluetooth
sudo systemctl enable bluetooth.service

# research
yay -S pandoc texlive-core texlive-latexextra texlive-fontsrecommended texlive-binextra texlive-mathscience texlive-plaingeneric texlive-bibtexextra miniconda3

# zsh
yay -S zsh zsh-syntax-highlighting zsh-autosuggestions && chsh -s $(which zsh)
sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
echo -e 'source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh' >> ~/.zshrc
echo -e 'source /usr/share/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.zsh' >> ~/.zshrc
echo -e '[ "$TERM" = "xterm-kitty" ] && alias ssh="kitty +kitten ssh"' >> ~/.zshrc
echo -e "bindkey '^H' backward-kill-word" >> ~/.zshrc
echo -e 'alias streamlink="streamlink --player mpv"' >> ~/.zshrc

# config git
git config --global user.name "Zhenhua Wang"
git config --global user.email "wang_zhenhua1996@icloud.com"
git config --global credential.helper store

# essential desktop
yay -S firefox kitty htop fastfetch ripgrep mpv yt-dlp streamlink file-roller exfat-utils

# flatpak applications
flatpak install com.github.tchx84.Flatseal com.github.johnfactotum.Foliate de.haeckerfelix.Fragments io.gitlab.adhami3310.Impression app.drey.Warp com.github.finefindus.eyedropper io.github.mrvladus.List com.obsproject.Studio org.gnome.Showtime org.gnome.Papers org.gimp.GIMP org.inkscape.Inkscape us.zoom.Zoom

# config
bash ~/.emacs.d/resources/scripts/zw-gnome-config.sh
bash ~/.emacs.d/resources/scripts/zw-gnome-keymap.sh
