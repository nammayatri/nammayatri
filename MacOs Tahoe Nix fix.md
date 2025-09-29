# macOS Tahoe Nix Fix

This document provides a step-by-step guide to fix Nix installation issues on macOS Tahoe.

## 1. Uninstall Nix and nix-darwin

```bash
# Uninstall Nix
/nix/nix-installer uninstall
sudo /nix/nix-installer uninstall
darwin-uninstaller
sudo darwin-uninstaller
sudo /nix/nix-installer uninstall
```

## 2. Clean up zsh configs

```bash
# Check for nix/starship references
grep -E 'nix|starship' ~/.zshrc ~/.zprofile /etc/zshrc

# Backup and clean zsh files
cp ~/.zshrc ~/.zshrc.bak
rm -f .zcompdump* .zprofile* .zshenv* .zlogin .zlogout .zshrc*
```

## 3. Remove /nix mount + profiles

```bash
# Remove profiles
sudo rm -rf /etc/profiles

# Check and backup synthetic.conf
cat /etc/synthetic.conf
sudo cp /etc/synthetic.conf /etc/synthetic.conf.bak

# Remove nix entry from synthetic.conf
sudo sed -i '' '/^nix$/d' /etc/synthetic.conf

# Reboot to apply changes
reboot
```

## 4. Verify uninstall

```bash
# Check if nix is properly removed
ls -ld /nix
ls -l /etc/nix
ls -l /etc/profiles
sudo launchctl list | grep nix
which nix
nix --version
```

## 5. Reinstall Nix

```bash
# Install Nix with daemon
curl -L https://nixos.org/nix/install | sh -s -- --daemon

# Source the profile
. /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh

# Verify installation
nix --version
nix doctor   # alias for `nix config check`
```

## 6. Enable flakes + nix-command

```bash
# Create nix config directory
mkdir -p ~/.config/nix

# Add experimental features
echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf
```

## 7. System-wide config

```bash
# Create system nix directory
sudo mkdir -p /etc/nix

# Create system nix.conf
cat <<'EOF' | sudo tee /etc/nix/nix.conf >/dev/null
experimental-features = nix-command flakes
trusted-users = root vijaygupta
max-jobs = auto
substituters = https://cache.nixos.org https://ny-ci-nixos.betta-gray.ts.net/
trusted-public-keys = ny-ci-nixos.betta-gray.ts.net:tjYdPZNppaGd6L9m7cMGzib4kkch1zAuR660dYp1DiY=
EOF

# Restart nix daemon
sudo launchctl kickstart -k system/org.nixos.nix-daemon

# Verify max-jobs setting
nix show-config | grep max-jobs
```

## 8. Install dev tools

```bash
# Install ormolu formatter
nix profile install nixpkgs#ormolu
ormolu --version
```

## 9. Bootstrap Home Manager via omnix

```bash
# Check omnix health
nix run nixpkgs#omnix -- health

# Initialize nixos-unified-template
nix run nixpkgs#omnix -- init github:juspay/nixos-unified-template -o ~/nixconfig

# Switch to home manager
cd ~/nixconfig
nix run home-manager/master -- switch --flake .#vijaygupta
```

## 10. Project setup with direnv + omnix

```bash
# Link environment file
ln -sf .envrc.backend .envrc

# Allow direnv
direnv allow
```

## 11. Final health check

```bash
# Verify everything is working
nix run nixpkgs#omnix -- health
```