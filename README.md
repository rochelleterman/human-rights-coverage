human-rights-coverage
=====================

Takes lexisnexis articles on human rights and analyzes them.

Rochelle Terman

University of California, Berkeley

rterman@gmail.com

Summary
-------

This project:

1. Takes a bunch of articles downloaded form Lexis Nexis (see below in "Before Starting")
2. Cleans and categorizes these articles; adds country, region, and subject metadata.
3. Does descriptive analysis, such as plotting number of articles by region over time.
4. Performs regression analysis on what factors determine more or less human rights coverage per country-year. **(To Come)**
5. Does automated text analysis on the corpora of articles, including comparative word distributions, td-idf, and some topic modeling. **(To Come)**


Before Starting
---------------

If you want to use this on your own data, you must first collect the data in the proper format. To do this:

1. Download a bunch of articles in Lexis Nexis
2. Parse them into a CSV. I used [Neal Caren's python script](http://nealcaren.web.unc.edu/cleaning-up-lexisnexis-files/) to do this.
3. Put the data in the /Data directory.

