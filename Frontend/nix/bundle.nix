{ ... }:
{
  imports = [
    ../node
  ];

  perSystem = { config, self', lib, pkgs, system, ... }:
    let
      make-bundle = { target, platform }:
        let
          webpack-config = "webpack.${platform}.js";
          dist-folder = "./dist/${platform}";
        in
        pkgs.stdenv.mkDerivation {
          name = "${target}-${platform}-bundle-js";
          phases = [ "buildPhase" "installPhase" ];
          nativeBuildInputs = [ self'.packages.nodejs ];
          buildPhase = ''
            ln -s ${self'.packages.nodeDependencies}/node_modules node_modules
            cp -r -L ${self'.packages.${target}}/output output
            cp ${ ../${target}/index.js } index.js
            cp ${ ../${target}/package.json } package.json
            cp ${ ../${target}/webpack.config.js } webpack.config.js
            cp ${ ../${target}/${webpack-config} } ${webpack-config}
            cp -r ${ ../${target}/src } src
            node node_modules/.bin/webpack --env prod --mode=production --progress --config ${webpack-config}
          '';
          installPhase = ''
            mv ${dist-folder}/index_bundle.js $out
          '';
        };

      bundle-options =
        lib.attrsets.cartesianProductOfSets {
          target = [ "ui-customer" "ui-driver" ];
          platform = [ "android" ]; # TODO: Re-add ios
        };

    in
    {
      packages =
        builtins.listToAttrs (map
          (args:
            let bundle = make-bundle args;
            in {
              inherit (bundle) name;
              value = bundle;
            })
          bundle-options);
    };
}
