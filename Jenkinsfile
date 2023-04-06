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
                sh 'nix build -L .#run-docker-compose'
                sh 'nix build -L .#run-mobility-stack'
            }
        }
        stage ('Cabal build') {
            steps {
                sh 'cd ./Backend && nix develop -c cabal build all'
            }
        }
        stage ('Flake check') {
            steps {
                sh 'nix build --no-link --no-update-lock-file -L .#check'
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
