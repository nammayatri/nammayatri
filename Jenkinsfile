pipeline {
  agent any

  environment {
    DEP_LABEL = "${sh(script: 'echo $JOB_BASE_NAME | tr \'/\' \'_\' | tr \'%2F\' \'_\' ', returnStdout: true)}"
  }

  stages {
    stage('Build & push dependancy') {
      when {
        anyOf {
          changeset "Jenkinsfile"
          expression { return hasChanged("Jenkinsfile") }
          changeset "Dockerfile"
          expression { return hasChanged("Dockerfile") }
          changeset "Makefile"
          expression { return hasChanged("Makefile") }
          changeset "stack.yaml"
          expression { return hasChanged("stack.yaml") }
          changeset "**/package.yaml"
          expression { return hasChanged("**/package.yaml") }
          expression {
            // build dep on the first run
            return (env.BUILD_NUMBER == "1")
          }
        }
      }

      steps {
        sh 'make build-dep -e IMAGE_NAME=beckn-epass -e DEP_LABEL=$DEP_LABEL'
        sh 'make push-dep -e IMAGE_NAME=beckn-epass -e DEP_LABEL=$DEP_LABEL'
      }
    }

    stage('Pipeline') {

      when {
        anyOf {
          branch "master"
          changeset "Jenkinsfile"
          expression { return hasChanged("Jenkinsfile") }
          changeset "Dockerfile"
          expression { return hasChanged("Dockerfile") }
          changeset "Makefile"
          expression { return hasChanged("Makefile") }
          changeset "lib/**/*"
          expression { return hasChanged("lib/**/*") }
          changeset "app/**/*"
          expression { return hasChanged("app/**/*") }
          changeset "stack.yaml"
          expression { return hasChanged("stack.yaml") }
          changeset "**/package.yaml"
          expression { return hasChanged("**/package.yaml") }
          changeset "deployment-configs/*deploy.yaml"
          expression { return hasChanged("deployment-configs/*deploy.yaml") }
        }
      }

      stages {

        stage('Docker build') {
          steps {
            sh 'make build -e VERSION=$(git rev-parse --short HEAD) -e DEP_LABEL=$DEP_LABEL'
          }
        }

        stage('Deploy') {
          when { branch "master" }

          stages {

            stage('Docker push') {
              steps {
                sh 'make push -e VERSION=$(git rev-parse --short HEAD)'
              }
            }

            stage('Deploy to staging') {
              environment {
                  BUILD_VERSION="""${sh(
                        returnStdout: true,
                        script: 'git rev-parse --short HEAD'
                    )}"""
              }

              steps {
                kubernetesDeploy(
                      kubeconfigId: 'jenkins-staging-deployer',
                      configs: 'deployment-configs/*deploy.yaml',
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

// `changeset` option for `when` directive in Jenkins pipeline has an issue,
// i.e., on the first build run, it doesn't find any file changes.
// Work around this using `git diff`
def boolean hasChanged(String glob) {
  // `changeset` also remembers the file changes between subsequent builds
  // This cannot be emulated using this work around, hence we only run this on first build run
  if (env.BUILD_NUMBER != "1") {
    return false
  }
  echo "First build, checking changes for ${glob}..."

  def MASTER = sh(
        returnStdout: true,
        script: "git rev-parse upstream/${env.CHANGE_TARGET}"
    ).trim()

  // Gets commit hash of HEAD commit. Jenkins will try to merge master into
  // HEAD before running checks. If this is a fast-forward merge, HEAD does
  // not change. If it is not a fast-forward merge, a new commit becomes HEAD
  // so we check for the non-master parent commit hash to get the original
  // HEAD. Jenkins does not save this hash in an environment variable.
  def HEAD = sh(
        returnStdout: true,
        script: "git show -s --no-abbrev-commit --pretty=format:%P%n%H%n HEAD | tr ' ' '\n' | grep -v ${MASTER} | head -n 1"
    ).trim()

  return !env.CHANGE_TARGET || sh(
    returnStatus: true,
    script: "[ -z \"\$(git diff --name-only ${MASTER}...${HEAD} -- ${glob})\" ] && exit 1 || exit 0"
  ) == 0
}