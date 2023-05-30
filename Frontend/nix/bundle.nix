{ ... }:
{
  imports = [
    ../node
  ];

  perSystem = { config, self', lib, pkgs, system, ... }:
    let
      make-bundle = { mode ? "production", env ? "prod", target, platform }:
        let
          webpack-config = "webpack.${platform}.js";
          dist-folder =
            if mode != "development" then
              "./dist/${platform}"
            else
              "./dist";
        in
        pkgs.stdenv.mkDerivation {
          name = "${target}-${platform}-${env}-${mode}-index-bundle-js";
          phases = [ "buildPhase" "installPhase" ];
          nativeBuildInputs = [ config.nammayatri.nodejs ];
          buildPhase = ''
            ln -s ${config.nammayatri.nodeDependencies}/node_modules node_modules
            cp -r -L ${self'.packages.${target}}/output output
            cp ${ ../${target}/index.js } index.js
            cp ${ ../${target}/package.json } package.json
            cp ${ ../${target}/webpack.config.js } webpack.config.js
            cp ${ ../${target}/${webpack-config} } ${webpack-config}
            node node_modules/.bin/webpack --env ${env} --mode=${mode} --progress --config ${webpack-config}
          '';
          installPhase = ''
            mv ${dist-folder}/index_bundle.js $out
          '';
        };

      bundle-options =
        lib.attrsets.cartesianProductOfSets {
          mode = [ "production" "development" ];
          env = [ "master" "sandbox" "prod" ];
          target = [ "ui-customer" "ui-driver" ];
          platform = [ "android" ]; # TODO: Re-add ios
        };

    in
    {
      packages =
        builtins.listToAttrs (map
          (args: {
            name = "${args.target}-${args.platform}-${args.env}-${args.mode}-js";
            value = make-bundle args;
          })
          bundle-options);
    };
}
