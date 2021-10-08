pipeline {
  agent any

  environment {
      BUILD_VERSION="""${sh(
            returnStdout: true,
            script: 'git rev-parse --short HEAD'
        )}"""
      BRANCH_NAME="""${sh(
            returnStdout: true,
            script: 'if [ "$BRANCH_NAME" = "release-version-7" ]; then echo "master"; else echo "$BRANCH_NAME"; fi;'
        )}"""
      DEPLOY_VARIANT="${env.BRANCH_NAME}"
  }

  stages {
    stage('Build & push dependency') {
      when {
        allOf {
          anyOf {
            branch "master"
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
          branch "sandbox"
          branch "production"
          changeRequest()
        }
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
              when { branch "master" }
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
