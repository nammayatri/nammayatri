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
        stage ('Cabal Build (Development)') {
            steps {
                sh 'cd ./Backend && nix develop -c cabal build all'
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
