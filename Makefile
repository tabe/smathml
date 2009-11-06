SCHEME = ypsilon --sitelib=sitelib

.PHONY: check test

check: test

test:
	$(SCHEME) tests/smathml/content.scm
	$(SCHEME) tests/smathml.scm
