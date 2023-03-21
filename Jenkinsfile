pipeline {
    agent any
    stages {
        stage ('Cachix setup') {
            steps {
                sh 'nix run nixpkgs#cachix use nammayatri'
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
            when { changeRequest branch: 'nixify' }
            environment {
              DOCKER_USER = credentials('docker-user')
              DOCKER_PASS = credentials('docker-pass')
            }
            steps {
                sh 'docker load -i $(nix build .#dockerImage --print-out-paths)'
                sh '''
                   IMAGE_NAME=$(nix eval --raw .#dockerImageName)
                   echo ${DOCKER_PASS} | docker login -u ${DOCKER_USER} --password-stdin ghcr.io
                   docker push ${IMAGE_NAME}
                   '''
            }
        }
        stage ('Push to cachix') {
          environment {
            CACHIX_AUTH_TOKEN = credentials('cachix-auth-token')
          }
          steps {
            sh 'nix run .#cachix-push'
          }
        }
    }
}
