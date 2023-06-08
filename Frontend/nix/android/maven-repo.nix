{ lib
, stdenv
, buildEnv
, fetchurl
, writeTextDir
, runCommand
, jq
, haskellPackages
}:

{ name ? "maven-deps"
, repos ? [ ]
, gradle-verification-metadata
, extraPaths ? [ ]
}:

with lib;

let
  mavenize = sep: replaceStrings [ "." ] [ sep ];

  deps-json = runCommand "deps.json" { } ''
    ${haskellPackages.xml-to-json-fast}/bin/xml-to-json-fast ${gradle-verification-metadata} \
      | ${jq}/bin/jq '.items[] | select( .name == "components") | .items | map( select (.name == "component") | .attrs + { artifacts: .items | map (select (.name = "artifact") | { (.attrs.name): .items[] | select (.name == "sha256") | .attrs.value }) | add })' > $out
  '';

  deps = builtins.fromJSON (builtins.readFile deps-json);

  fetch =
    { group
    , name
    , version
    , file
    , sha256
    }:
    fetchurl {
      name = file;
      urls = map (repo: "${repo}/${mavenize "/" group}/${name}/${version}/${file}") repos;
      inherit sha256;
      meta.platforms = platforms.all;
    };

  fetchDependency =
    { group
    , name
    , version
    , artifacts
    }:
    let
      fetchArtifact = file: sha256:
        fetch { inherit group name version file sha256; };

      # Each artifact uses the filename in the Gradle cache, which doesn't
      # correspond to the filename in the Maven repo. The mapping of name to URL
      # is provided by Gradle module metadata, so we fetch that first. See
      # https://github.com/gradle/gradle/blob/master/subprojects/docs/src/docs/design/gradle-module-metadata-latest-specification.md
      # for the file format.
      isModule = hasSuffix ".module";
      moduleArtifacts = filterAttrs (file: _: isModule file) artifacts;
      otherArtifacts = filterAttrs (file: _: !isModule file) artifacts;

      modules = mapAttrsToList fetchArtifact moduleArtifacts;

      replacements = listToAttrs (flatten (map
        (module:
          let
            json = builtins.fromJSON (builtins.readFile module);
            variants = json.variants or [ ];
            files = flatten (map (v: v.files or [ ]) variants);
          in
          map ({ name, url, ... }: nameValuePair name url) files
        )
        modules));

      replaced = mapAttrs'
        (file:
          nameValuePair (replacements.${file} or file)
        )
        otherArtifacts;
    in
    if moduleArtifacts == { }
    then mapAttrsToList fetchArtifact artifacts
    else modules ++ (mapAttrsToList fetchArtifact replaced);

  mkDep =
    { group
    , name
    , version
    , artifacts
    }@dep:
    stdenv.mkDerivation {
      pname = "${mavenize "-" group}-${name}";
      inherit version;

      srcs = fetchDependency dep;

      sourceRoot = ".";

      phases = "installPhase";

      enableParallelBuilding = true;
      preferLocalBuild = true;

      installPhase = ''
        dest=$out/${mavenize "/" group}/${name}/${version}
        mkdir -p $dest
        for src in $srcs; do
          cp $src $dest/$(stripHash $src)
        done
      '';
    };

  mkMetadata = deps:
    let
      modules = groupBy'
        (meta: { group, name, version, ... }:
          let
            isNewer = versionOlder meta.latest version;
            isNewerRelease = versionOlder meta.release version;
          in
          {
            groupId = group;
            artifactId = name;
            latest = if isNewer then version else meta.latest;
            release = if isNewerRelease then version else meta.release;
            versions = meta.versions ++ [ version ];
          }
        )
        {
          latest = "";
          release = "";
          versions = [ ];
        }
        ({ group, name, ... }: "${mavenize "/" group}/${name}/maven-metadata.xml")
        deps;
    in
    attrValues (mapAttrs
      (path: { groupId, artifactId, latest, release, versions }:
        let
          versions' = sort versionOlder (unique versions);
        in
        writeTextDir path ''
          <?xml version="1.0" encoding="UTF-8"?>
          <metadata modelVersion="1.1">
            <groupId>${groupId}</groupId>
            <artifactId>${artifactId}</artifactId>
            <versioning>
              ${optionalString (latest != "") "<latest>${latest}</latest>"}
              ${optionalString (release != "") "<release>${release}</release>"}
              <versions>
                ${concatMapStringsSep "\n    " (v: "<version>${v}</version>") versions'}
              </versions>
            </versioning>
          </metadata>
        ''
      )
      modules);

  mkGradleRedirectionPoms = deps:
    let
      depsMissingPoms = filter
        ({ artifacts, ... }:
          any (hasSuffix ".module") (attrNames artifacts) &&
          !(any (hasSuffix ".pom") (attrNames artifacts))
        )
        deps;
    in
    map
      ({ group, name, version, ... }:
        writeTextDir "${mavenize "/" group}/${name}/${version}/${name}-${version}.pom" ''
          <project xmlns="http://maven.apache.org/POM/4.0.0"
                   xsi:schemaLocation="http://maven.apache.org/POM/4.0.0
                   http://maven.apache.org/xsd/maven-4.0.0.xsd"
                   xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
            <!-- This module was also published with a richer model, Gradle metadata,  -->
            <!-- which should be used instead. Do not delete the following line which  -->
            <!-- is to indicate to Gradle or any Gradle module metadata file consumer  -->
            <!-- that they should prefer consuming it instead. -->
            <!-- do_not_remove: published-with-gradle-metadata -->
            <modelVersion>4.0.0</modelVersion>
            <groupId>${group}</groupId>
            <artifactId>${name}</artifactId>
            <version>${version}</version>
          </project>
        ''
      )
      depsMissingPoms;

in
buildEnv {
  inherit name;
  paths = map mkDep deps ++ mkMetadata deps ++ mkGradleRedirectionPoms deps ++ extraPaths;
}
