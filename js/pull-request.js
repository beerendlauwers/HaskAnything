


function writeToConsole( content ) {
    console.log(content);
}

function writeToHtmlArea( content ) {
    var selector = jQuery('#submit-pull-request-feedback');
    
    selector.val( selector.val() + '\n' + content );
    writeToConsole( content );
}

function collectDataAndSubmitPullRequest(templateName,titleSelector) {
    
    var github = OAuth.create('github');
    
    if (!github) {
        writeToHtmlArea( "You're not logged into Github. Log in first at the top right of the page." );
        return;
    }
    
    var userToken = github.access_token;
    var githubData = getGithubData();
    var userName = githubData.alias;
    
    var data = {};
    data.fileContents = generateFilePreview(templateName);
    data.fileType = templateName;
    data.fileName = getFinalFileTitle(templateName,titleSelector);
    data.commitMessage = "Committed file '" + data.fileName + "' via the Hask Anything web interface.";
    data.pullRequestName = "Automatic pull request for " + templateName + " " + jQuery(titleSelector).val();
    data.pullRequestBody = "Created via the Hask Anything web interface. If this contains copyrighted material, please ask the author if we are allowed to replicate it here with attribution.";
    
    submitHaskAnythingPullRequest( userToken, userName, data, writeToHtmlArea );
}

function onPullRequestSuccessful() {
    // Disable the pull request button.
    jQuery('#submit-pull-request-button').prop('disabled','disabled');
    
    // Make the "want to submit again?" button visible.
    jQuery('#new-submit-request-button').show();
}

function naiveHash(str) {
  var hash = 0, i, chr, len;
  if (str.length === 0) return hash;
  for (i = 0, len = str.length; i < len; i++) {
    chr   = str.charCodeAt(i);
    hash  = ((hash << 5) - hash) + chr;
    hash |= 0; // Convert to 32bit integer
  }
  return hash;
}

function submitHaskAnythingPullRequest( userToken, userName, data, logWriter ) {
    
    console.log(userToken);
    
    // Open up the connection to Github.
    var github = new Github({
        token: userToken,
        auth: "oauth"
    });
    
    logWriter("Opened up a connection to Github.");
    
    // This is the repository that will be forked and a pull request submitted against.
    var master = { username: "beerendlauwers", repo: "HaskAnything" };
    
    var masterHaskAnything = github.getRepo(master.username, master.repo);
    
    // This is the branch that we will be working in for this particular document.
    // We need a separate branch for each pull request, otherwise a single pull request would get polluted with a bunch of commits.
    var forkBranchName = "document-" + userName + "-" + data.fileType + "-" + naiveHash(data.fileContents);
    
    // First, check if we've already forked it.
    var maybeFork = github.getRepo(userName, master.repo);
    
    logWriter("Checking if we've already forked repository '" + master.repo + "' from " + master.username + " ...");
    
    maybeFork.show( function(error,repo) {
        if (error) {
            // No such repository is known, so let's fork it and restart.
            logWriter("No fork found. Forking...");
            
            masterHaskAnything.fork( function(error) {
                if (error) {
                    logWriter("Something went wrong during the forking of the '" + master.repo + "' repository. The error data follows:");
                    logWriter(error);
                }
            });
            
            logWriter("Forked repository '" + master.repo + "' from '" + master.username + "'.");
            logWriter("We'll start again from the top in three seconds.");
            logWriter("3..");
            setTimeout( function() { logWriter("2.."); }, 1000 );
            setTimeout( function() { logWriter("1.."); }, 2000 );
            setTimeout( function() { submitHaskAnythingPullRequest( userToken, userName, data, logWriter ); }, 3000 );
            return;
        }
        else if (repo !== undefined) {
            // Repository seems to be forked.
            logWriter("Repository seems to be forked.");
            
            var justFork = maybeFork;
            
            // Check if we already have a fresh branch for this pull request.
            justFork.listBranches( function(error,branches) {
                
                var commitAndPullRequestCode = function() {
                    logWriter("Checking if we've already written this data to this file...");
                        
                        // Check if we've already committed these *exact* file contents to this *exact* file name.
                        justFork.read(forkBranchName, data.fileName, function( error, result ) {
                            var skipCommit = false;
                        
                            if (error && error !== "not found") {
                                logWriter("Something went wrong during the writing of the commit. The error data follows:");
                                logWriter('"' + error + '"');
                                return;
                            }
                            else if (result && result === data.fileContents) {
                                logWriter("File \"" + data.fileName + "\" already contains this data. Skipping the commit.");
                                skipCommit = true;
                            }
                            
                            var doPullRequest = function() {
                                // Ok, create a pull request for this commit.
                                var pull = {
                                  title: data.pullRequestName,
                                  body: data.pullRequestBody,
                                  base: "master",
                                  head: userName + ":" + forkBranchName
                                };
                                
                                // See if there's already a pull request open for this branch.
                                masterHaskAnything.listPulls('open', function(error, pullRequests) {
                                   
                                   if ( error === null) {
                                       var f = function( pullRequestObj ) {
                                           return pullRequestObj.head.ref === forkBranchName;
                                       };
                                       var results = R.filter(f, pullRequests);
                                       
                                       if ( results.length === 0 ) {
                                        logWriter("Creating pull request \"" + pull.title + "\"...");
                                        
                                        masterHaskAnything.createPullRequest( pull, function(error, pullRequest) {
                                            if (error) {
                                                logWriter("Something went wrong during the creation of the pull request. The error data follows:");
                                                
                                                if (error.error == 422) {
                                                    logWriter(error.request.responseText);
                                                }
                                                
                                                logWriter(error); 
                                            }
                                            else {
                                                logWriter("Pull request \"" + pull.title + "\" created.");
                                                logWriter("Done!");
                                                onPullRequestSuccessful();
                                            }
                                        });
                                       }
                                       else {
                                           logWriter("A pull request for branch '" + forkBranchName + "' already exists. Skipping.");
                                       }
                                   }
                                   else {
                                       logWriter("Something went wrong while trying to get the list of pull requests. The error data follows:");
                                       logWriter(error);
                                   }
                                });
                                

                            }
                                
                            if (!skipCommit) {
                                // Let's write a commit to the forked repository now.
                                logWriter("Writing the commit '" + data.commitMessage + "' to the '" + forkBranchName + "' branch of the fork...");
                                
                                justFork.write(forkBranchName, data.fileName, data.fileContents, data.commitMessage, function(err) {
                                    if (error && error !== "not found") {
                                        logWriter("Something went wrong during the writing of the commit. The error data follows:");
                                        logWriter(error);
                                        return;
                                    }
                                    else {
                                        logWriter("Commit '" + data.commitMessage + "' written.");
                                        doPullRequest();
                                    }
                                });
                            }
                            else {
                                doPullRequest();
                            }
                        });
                }
                
                if ( R.contains(forkBranchName, branches) ) {
                    // We already have a branch for this pull request.
                    logWriter("Branch " + forkBranchName + " already exists.");
                    commitAndPullRequestCode();
                }
                else {
                    // Create a new branch for this document.
                    justFork.branch("master",forkBranchName, function(error,something) {
                        
                        if ( error !== null ) {
                            logWriter("Something went wrong during the branching of the master branch of the forked repository to a new branch called '" + forkBranchName + "'. The error data follows:");
                            logWriter(error);
                            return;
                        }
                        else {
                            commitAndPullRequestCode();
                        }
                    });
                }              
            });
        }
        else {
            logWriter("Something went wrong during while checking if the '" + master.repo + "' repository was forked. The error data follows:");
            logWriter(error);
        }
    });
}

function testSubmit() {
    var hask = {
        commitMessage: "Testing the entire flow",
        fileName: "flowtest.md",
        fileContents: "------\ntitle: Testing the flow\n------\nThis should work",
        pullRequestName: "Automated pull request with flow",
        pullRequestBody: "Body of the pull request."
    };
}
