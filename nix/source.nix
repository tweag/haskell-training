{
  nixpkgs = import
    (builtins.fetchTarball {
      name = "nixos-release-22.05";
      url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.05.tar.gz";
      sha256 = "0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik";
    })
    { };
}
