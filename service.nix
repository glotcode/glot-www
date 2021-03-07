{ config, lib, pkgs, ... }:

let
  glot =
    (import ./default.nix {}).glot.components.exes.glot;

  cfg =
    config.services.glot;

  commonEnvironment = {
    LC_ALL = "en_US.UTF-8";
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  };
in
{
  options = {
    services.glot = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether to enable glot";
      };

      enablePostgres = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether to enable postgres";
      };

      environment = lib.mkOption {
        type = lib.types.attrs;
        default = {};
        description = "Environment variables for the service";
      };
    };
  };


  config = lib.mkMerge [
    (
      lib.mkIf cfg.enable {
        # Service user
        users.extraUsers.glot = {
          isNormalUser = true;
          description = "glot service user";
        };

        # Systemd service
        systemd.services.glot = {
          description = "glot";
          wantedBy = [ "multi-user.target" ];
          requires =
            if cfg.enablePostgres then
              [ "postgresql.service" ]

            else
              [];

          serviceConfig =
            {
              WorkingDirectory = "${cfg.environment.WORK_DIR}";
              ExecStart = "${glot}/bin/glot";
              Restart = "always";
              User = "glot";
            };

          environment = commonEnvironment // cfg.environment;
        };
      }
    )

    (
      lib.mkIf cfg.enablePostgres {
        # Database
        services.postgresql = {
          enable = true;
          package = pkgs.postgresql_12;
          enableTCPIP = true;
          authentication = lib.mkOverride 10 ''
            local all all trust
            host all all ::1/128 trust
          '';

          initialScript = pkgs.writeText "backend-initScript" ''
            CREATE ROLE ${cfg.environment.DB_USER} WITH LOGIN PASSWORD '${cfg.environment.DB_PASS}' CREATEDB;
            CREATE DATABASE ${cfg.environment.DB_NAME};
            GRANT ALL PRIVILEGES ON DATABASE ${cfg.environment.DB_NAME} TO ${cfg.environment.DB_USER};
          '';
        };
      }
    )
  ];
}
