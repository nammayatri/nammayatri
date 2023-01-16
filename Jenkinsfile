pipeline {
  agent {
    label 'beckn'
  }

  environment {
      BUILD_VERSION="""${sh(
            returnStdout: true,
            script: 'git rev-parse --short HEAD'
        )}"""
      DEPLOY_VARIANT = overrideBranchName()
  }

  stages {
    stage('Build & push dependency') {
      when {
        allOf {
          anyOf {
            branch "master"
            branch "release-version-7"
            branch "sandbox"
            branch "production"
            branch "feature/log-processor"
          }
          anyOf {
            changeset "Jenkinsfile"
            changeset "Dockerfile*"
            changeset "Makefile"
            changeset "stack.yaml"
            changeset "stack.yaml.lock"
            changeset "**/package.yaml"
          }
        }
      }

      environment {
          BRANCH_NAME= overrideBranchName()
      }

      steps {
        sh 'make build-dep'
        sh 'make push-dep -e IMAGE_REPO=SANDBOX'
        sh 'make push-dep -e IMAGE_REPO=PRODUCTION'
      }
    }

    stage('Pipeline') {
      when {
        anyOf {
          branch "master"
          branch "release-version-7"
          branch "sandbox"
          branch "production"
          branch "feature/log-processor"
          changeRequest()
        }
      }

      environment {
          BRANCH_NAME= overrideBranchName()
      }

      stages {
        stage('Docker build') {
          steps {
            sh '/usr/bin/aws s3 cp s3://jp-artifacts-archive/builds/log-processor/7f2eae2/bin/log-processor .'
            sh 'chmod +x log-processor'
            sh 'make build'
          }
        }

        stage('Deployment stage') {
          when {
            anyOf {
              branch "master"
              branch "release-version-7"
              branch "sandbox"
              branch "production"
            }
          }

          stages {

            stage('Docker push') {
              steps {
                sh 'make push -e IMAGE_REPO=SANDBOX'
                sh 'make push -e IMAGE_REPO=PRODUCTION'
              }
            }

          }
        }

      }
    }

  }
}

def overrideBranchName() {
    def branchName = "${env.BRANCH_NAME}"
    if (branchName == "release-version-7") {
        return 'master'
    }
    else {
        return branchName
    }
}