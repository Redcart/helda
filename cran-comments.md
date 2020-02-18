## Test environments
  * macOS 10.12.6 (on travis-ci)
     - release
  * Ubuntu Xenial 16.04 (on travis-ci)
     - oldrel, release, devel
  * win-builder
     - oldrel, release, devel_gcc8

## R CMD check results
  There were no ERRORs no WARNINGs no Notes


## For local macOS 10.12.6 only 
  - release
     
## R CMD check results
  Note: found 1 marked UTF-8 string
  
It seems to concern the checking of the data sets I include in the package and esoeccially the encoding
UTF-8 vs ASCII.
I tried to encode them in ascii and resaved them with compression but the note still comes out.
  
