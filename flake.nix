{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/23.11";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    let system = "x86_64-linux";
    in
    {
      nixosConfigurations =
        let
          lib = nixpkgs.lib;
          getHostFQDN = nixosConfig:
            let
              toplevelStorePath = nixosConfig.config.system.build.toplevel.drvPath;
              fileName = lib.elemAt (lib.splitString "/" toplevelStorePath) 3;
              hash = lib.elemAt (lib.splitString "-" fileName) 0;
            in
            "${hash}.hash.garnix.me";
          simpleServerModule =
            ({ pkgs, config, lib, ... }:
              {
                options = {
                  server = {
                    executable = lib.mkOption {
                      type = lib.types.str;
                    };
                  };
                };

                config = {
                  nixpkgs.hostPlatform = system;
                  system.stateVersion = "23.11";
                  fileSystems."/" = { device = "/dev/sda1"; fsType = "ext4"; };
                  boot.loader.grub.device = "/dev/sda";

                  virtualisation.vmVariant = {
                    virtualisation.graphics = false;
                    services.getty.autologinUser = "root";
                  };

                  networking.firewall = {
                    enable = true;
                    allowedTCPPorts = [ 80 ];
                  };

                  users.mutableUsers = false;

                  users.users.shahn = {
                    isNormalUser = true;
                    description = "Sönke Hahn";
                    extraGroups = [ "wheel" "systemd-journal" ];
                    openssh.authorizedKeys.keys = [
                      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCzsYv/IpqFuE29NVBQrslVqvdeEdPVfQqSg1pVyTh40j2Z3UK8uK6fCSLGyQZNsqyO5B8785tqLL9MoVJMfqVPhSUiRZqXvjMFXuxCTqV5YndXc8qFNfjgPxVGWUrZQsGpFQKj8LAbSXjxdBKFZvuU9/vo9GlxBUhcKdDLax4r/OqGOBSIRb5Cgwt2i85Yi1uB5hivdTL28Csx19IlmlAxJyRRltxOetC2eD9jF3qRQQciz/CjXUSGNKcyI2PhnCpeoH9v7j2+UrTsyN0JVGfMJoOvYW97QE3vYvefK1VGWnU8BrS3ybW4c4snHDr5OzaBNfNkmw765bM89HRiTL+HBbkGx1f739UCdcZnYiUzZBKoJRw4J4XqlIyuApCRrRUOG8PBPcClh1kldMxeJxpmGmIIdvOh++kIffOkOfCnEZUVlmqwLxeeYMZTPJ13yL9bQis1vR2dqeNud25eyK1FbaMTt5GE08Zcg/j39YBLxz/0hK4uE3bQbOA+eCEgypU= shahn@lissabon"
                    ];
                  };

                  services.openssh.enable = true;

                  systemd.services.server = {
                    description = "test server";
                    wantedBy = [ "multi-user.target" ];
                    after = [ "network-online.target" ];
                    serviceConfig = {
                      Type = "simple";
                      User = "root";
                      ExecStart = builtins.toString config.server.executable;
                    };
                  };

                  services.nginx = {
                    enable = true;
                    virtualHosts."_".locations."/".proxyPass = "http://localhost:8080";
                  };
                };
              });
        in
        {
          frontend = lib.nixosSystem {
            modules = [
              simpleServerModule
              {
                config = {
                  server.executable = self.apps.${system}.frontend.program;
                  systemd.services.server.serviceConfig.Environment = [
                    "BACKEND_URL=\"http://${getHostFQDN self.nixosConfigurations.backend}\""
                  ];
                };
              }
            ];
          };
          backend = lib.nixosSystem {
            modules = [
              simpleServerModule
              { config.server.executable = self.apps.${system}.backend.program; }
            ];
          };
        };
    } //
    flake-utils.lib.eachSystem [ system ] (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        mkHaskellApp = file:
          {
            type = "app";
            program =
              builtins.toString (pkgs.runCommand "haskell-programm"
                {
                  buildInputs = [
                    (pkgs.haskellPackages.ghc.withPackages (p: [
                      p.string-conversions
                      p.wai
                      p.warp
                      p.wreq
                    ]))
                  ];
                }
                ''
                  ghc ${file} -o main
                  cp ./main $out
                '');
          };
      in
      rec {
        apps = {
          vm = {
            type = "app";
            program = pkgs.lib.getExe (pkgs.writeShellApplication {
              name = "vm";
              text = ''
                host=$1
                result=$(nix build -L ".#nixosConfigurations.$host.config.system.build.vm" --no-link --print-out-paths)
                rm -f nixos.qcow2
                exec "$result/bin/run-nixos-vm"
              '';
            });
          };

          backend = mkHaskellApp ./src/backend.hs;
          frontend = mkHaskellApp ./src/frontend.hs;
        };
      }
    );
}
