etc
---

My personal dotfiles for NixOS.

To install it:
- Provision a new system.
- Set up flakes.
- Set up agenix.
- Reverse Curve25519 encryption to get my keys, and put them in `/etc/agenix`.
- Set the right hostname.
- `git clone https://git.bicompact.space/hazel/etc /etc/nixos`
- `sudo nixos-rebuild switch`

Quirks:
- I compile Racket from source, in `~/src/racket`. The Racket module just sets the right `$LD_LIBRARY_PATH`.
- A few proprietary packages need to be installed manually (Citrix Workspace, Mathematica).
- DOOM Emacs needs to be installed manually.
- Emacs is currently pinned to a build from October. Circumstances.
