pipeline {
  agent {
    docker {
      image 'haskell'
    }

  }
  stages {
    stage('Prepare Environment') {
      steps {
        sh '''ghc --version
stack clean'''
      }
    }

    stage('Build') {
      steps {
        sh 'stack build'
      }
    }

    stage('Test') {
      steps {
        sh 'stack test'
      }
    }

    stage('Clean up') {
      steps {
        sh 'stack clean'
        cleanWs(cleanWhenAborted: true, cleanWhenFailure: true, cleanWhenNotBuilt: true, cleanWhenSuccess: true, cleanWhenUnstable: true, cleanupMatrixParent: true, deleteDirs: true)
      }
    }

  }
}