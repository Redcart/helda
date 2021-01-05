## Test environments
  * macOS Catalina (on githubActions)
     - oldrel, release
  * Ubuntu Xenial 18.04 (on githubActions)
     - oldrel, release, devel
  * win-builder
     - oldrel, release, devel

## R CMD check results
  There were no ERRORs no WARNINGs no Notes

except for macOS Catalina (on githubActions)
- devel 
  
Error: Package suggested but not available: 
  ‘devtools’
It seems that environment provided by githubActions have a problem but this does not come from my helda package.
You can see the corresponding log at:
https://github.com/Redcart/helda/runs/1548658565?check_suite_focus=true

## New version for problem with github repository link that has changed from https://www.github.com/Redcart/helda to https://github.com/Redcart/helda
