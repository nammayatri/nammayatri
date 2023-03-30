pipeline {
    agent any
    stages {
        stage ('Cachix setup') {
            steps {
                cachixUse 'nammayatri'
            }
        }
        stage ('Nix Build') {
            steps {
                sh 'nix build -L .#nammayatri'
            }
        }
        // Nix doesn't run all tests, so we must run them via cabal.
        stage ('Cabal Tests') {
            steps {
                sh '''
                    set -ex
                    cd ./Backend 
                    cp -f dhall-configs/dev/secrets/top-secret-template.dhall dhall-configs/dev/secrets/top-secret.dhall
                    nix develop -c cabal test all
                   '''
            }
        }
        stage ('Flake check') {
            steps {
                sh 'nix build -L .#check'
            }
        }
        stage ('Docker image') {
            when { branch 'main' }
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
