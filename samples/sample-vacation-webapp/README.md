Run with mvn spring-boot:run

Available at http://localhost:8079

Use the same users as in Flowable for the managers

Use in conjunction to the launcher-standalone-flowable test:
 - put an `<input />` tag in the build.xml ant script just after the antcall to waitWebservices so that execution stops before the integration tests are executed
 - from there you can play with the webapp
