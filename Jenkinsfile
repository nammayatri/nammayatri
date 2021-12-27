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
          changeRequest()
        }
      }

      environment { 
          BRANCH_NAME= overrideBranchName()
      }

      stages {
        stage('Docker build') {
          steps {
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

            stage('Apply configs') {
              when {
                anyOf {
                  branch "master"
                  branch "release-version-7"
                  branch "sandbox"
                }
              }

              steps {
                sh '''
                  (kubectl create configmap beckn-dhall-config-$DEPLOY_VARIANT \
                    --from-file=dhall-configs/$DEPLOY_VARIANT \
                    --from-file=globalCommon.dhall=dhall-configs/generic/common.dhall \
                    -n atlas -o yaml --dry-run | \
                    grep -v 'creationTimestamp' \
                  ) > deployment-configs/sandbox/beckn-dhall-config.yaml
                '''
                kubernetesDeploy(
                      kubeconfigId: 'jenkins-baby-hulk-deployer',
                      configs: 'deployment-configs/sandbox/beckn-dhall-config.yaml',
                      enableConfigSubstitution: true
                    )
                kubernetesDeploy(
                      kubeconfigId: 'jenkins-baby-hulk-deployer',
                      configs: 'deployment-configs/sandbox/*deploy.yaml',
                      enableConfigSubstitution: true
                    )
              }
            }

            stage('Deploy master') {
              when {
                anyOf {
                  branch "master"
                  branch "release-version-7"
                }
              }
              steps {
                kubernetesDeploy(
                      kubeconfigId: 'jenkins-baby-hulk-deployer',
                      configs: 'deployment-configs/sandbox/*deploy.yaml',
                      enableConfigSubstitution: true
                    )
              }
            }

            stage('Deploy sandbox') {
              when { branch "sandbox" }
              steps {
                kubernetesDeploy(
                      kubeconfigId: 'jenkins-baby-hulk-deployer',
                      configs: 'deployment-configs/sandbox/app-backend-deploy.yaml',
                      enableConfigSubstitution: true
                    )
                kubernetesDeploy(
                      kubeconfigId: 'jenkins-baby-hulk-deployer',
                      configs: 'deployment-configs/sandbox/beckn-gateway-deploy.yaml',
                      enableConfigSubstitution: true
                    )
                kubernetesDeploy(
                      kubeconfigId: 'jenkins-baby-hulk-deployer',
                      configs: 'deployment-configs/sandbox/beckn-transport-deploy.yaml',
                      enableConfigSubstitution: true
                    )
                kubernetesDeploy(
                      kubeconfigId: 'jenkins-baby-hulk-deployer',
                      configs: 'deployment-configs/sandbox/beckn-transport-allocation-service-deploy.yaml',
                      enableConfigSubstitution: true
                    )
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