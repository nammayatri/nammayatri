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
                sh 'nix build --no-link --no-update-lock-file -L .#nammayatri'
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
