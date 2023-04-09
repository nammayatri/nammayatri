pipeline {
    agent any
    stages {
        stage ('Cachix setup') {
            steps {
                cachixUse 'nammayatri'
            }
        }
        stage ('Nix Build All') {
            steps {
                // TODO: Upstream to jenkins-nix-ci
                sh '''
                  set -x
                  for DRV in $(nix run --refresh github:srid/flake-outputs)
                  do
                    nix build .#"$DRV"
                  done
                '''
            }
        }
        stage ('Cabal build') {
            steps {
                sh 'cd ./Backend && nix develop -c cabal build all'
            }
        }
        stage ('Flake check') {
            steps {
                sh 'nix flake check --allow-import-from-derivation --override-input systems github:nix-systems/x86_64-linux'
            }
        }
        stage ('Docker image') {
            when {
                branch 'main'
            }
            steps {
                dockerPush "dockerImage", "ghcr.io"
            }
        }
        stage ('Cachix push') {
            steps {
                cachixPush "nammayatri"
            }
        }
    }
}
