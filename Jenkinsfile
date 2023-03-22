pipeline {
    agent any
    stages {
        stage ('Cachix setup') {
            steps {
                sh 'cachix use nammayatri'
            }
        }
        stage ('Nix Build') {
            steps {
                sh 'nix build -L .#nammayatri'
            }
        }
        stage ('Flake check') {
            steps {
                sh 'nix build -L .#check'
            }
        }
        stage ('Docker image') {
            when { branch 'main' }
            environment {
              DOCKER_USER = credentials('docker-user')
              DOCKER_PASS = credentials('docker-pass')
              DOCKER_SERVER = 'ghcr.io'
            }
            steps {
                sh 'nix run github:juspay/jenkins-nix-ci#docker-push dockerImage'
            }
        }
        stage ('Push to cachix') {
          environment {
            CACHIX_AUTH_TOKEN = credentials('cachix-auth-token')
          }
          steps {
            sh 'nix run github:juspay/jenkins-nix-ci#cachix-push nammayatri'
          }
        }
    }
}
