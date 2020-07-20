{ config, lib, pkgs, ... }:
with lib; let
  cfg = config.hazel.modules.i3status-rust;

  mapAttrsToList = f: l:
    attrValues (mapAttrs f l);

  formatLine = n: v:
    let
      formatValue = v:
        if isList v then
          "[${concatStringsSep ", " (map (x: ''"${x}"'') v)}]"
        else if isBool v then
          (if v then "true" else "false")
        else if isString v then
          ''"${v}"''
        else
          toString v; in
      "${n} = ${formatValue v}";

  formatAttrs = vs:
    concatStringsSep "\n" (mapAttrsToList formatLine vs);

  formatBlock = vs:
    ''
      [[block]]
      block = "${vs.name}"
      ${formatAttrs vs.settings}
    '';

  formatSetting = b: s: vs:
    ''
      [${b}]
      name = "${s}"
      [${b}.overrides]
      ${formatAttrs vs}
    '';
in {
  options = {
    hazel.modules.i3status-rust = {
      enable = mkEnableOption "i3status-rust";

      blocks = mkOption {
        type = types.listOf (types.submodule {
          options = {
            name = mkOption {
              type = with types; str;
              description = ''
                The name of the block.
              '';
            };

            settings = mkOption {
              default = {};
              type = with types; attrsOf (oneOf [ bool int str (listOf str) ]);
              description = ''
                The settings for this block.
              '';
            };
          };
        });
      };

      icons = mkOption {
        type = types.submodule {
          options = {
            name = mkOption {
              default = "awesome";
              type = with types; str;
              description = ''
                The icon pack to use for the bar.
              '';
            };

            overrides = mkOption {
              default = {};
              type = with types; attrsOf str;
              description = ''
                Overridden icons for use in the given block.
              '';
            };
          };
        };
      };

      theme = mkOption {
        type = types.submodule {
          options = {
            name = mkOption {
              default = "solarized-dark";
              type = with types; str;
              description = ''
                The theme to use in the bar.
              '';
            };

            overrides = mkOption {
              default = {};
              type = with types; attrsOf str;
              description = ''
                Overridden colors for use in the given theme.
              '';
            };
          };
        };
      };
    };
  };

  config = mkIf cfg.enable {
    hazel.home = {
      home.packages = [ pkgs.i3status-rust ];

      xdg.configFile."i3status-rust/status.toml" = {
        text = concatStringsSep "\n"
          ([ (formatSetting "icons" cfg.icons.name cfg.icons.overrides)
             (formatSetting "theme" cfg.theme.name cfg.theme.overrides) ]
          ++ map formatBlock cfg.blocks);
        onChange = "swaymsg reload";
      };
    };
  };
}
