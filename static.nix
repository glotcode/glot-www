{ pkgs ? import <nixpkgs> {} }:
let
  src =
    ./.;

  cmd =
    ''
    mkdir -p $out
    cp -rvf ${src}/static/* $out/
    '';
in
pkgs.runCommand "glot-static" {} cmd
