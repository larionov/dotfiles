# arr

System configuration for the media stack: sonarr, sabnzbd, jellyfin.

Stowed to `/` rather than `~`, the same way the `udev` package is:

    cd ~/dotfiles && sudo stow -v -t / arr

## What it does

All three services run as separate users and need to read and write the same
files. They share the `media` group, all use umask 002, and every directory
under `/mnt/arr` carries the setgid bit, so anything created there ends up
group `media` with mode 664 for files and 2775 for directories.

This follows the TRaSH Guides native layout:
https://trash-guides.info/File-and-Folder-Structure/How-to-set-up/Native/

## Layout

    /mnt/arr/
    |-- media/
    |   `-- tv/                 sonarr root folder, jellyfin library
    `-- usenet/
        |-- incomplete/         sabnzbd work area
        `-- complete/tv/        sabnzbd hands off, sonarr imports

Both halves sit under one parent on one filesystem, so imports are atomic
renames rather than copy plus delete.

## Why these files survive pacman

- `etc/systemd/system/*.service.d/override.conf` are drop-ins. Package updates
  replace the unit in `/usr/lib/systemd/system`, never the drop-in.
- `etc/sysusers.d/10-arr-media.conf` re-applies group membership on every
  `systemd-sysusers` run, including the one each package update triggers.
- `etc/tmpfiles.d/10-arr-media.conf` re-applies directory ownership and modes
  on every boot.

## Settings that live in the apps, not here

- SABnzbd, Config > Folders: permissions `775`, incomplete
  `/mnt/arr/usenet/incomplete`, complete `/mnt/arr/usenet/complete`.
  SABnzbd overrides the process umask, so this field is the knob that matters.
- Sonarr root folder: `/mnt/arr/media/tv`.
- Jellyfin library: `/mnt/arr/media/tv`.
