/**
 * Copyright (c) 2014 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn;

/**
 * The component class of the Activiti BPMN Service Engine related constants.
 * 
 * @author Bertrand Escudie - Linagora
 */
public final class ActivitiSEConstants {

    /**
     * The SU parameter that defines the tenantId.
     */
    public static final String TENANT_ID = "tenant_id";
    	
	 /**
     * The SU parameter that defines the categoryId
     */
    public static final String CATEGORY_ID = "category_id";
    	
    /**
     * The SU parameter that defines the bpmn20.xml file.
     */
    public static final String PROCESS_FILE = "process_file";
    	
	 /**
     * The SU parameter that defines the version
     */
    public static final String VERSION = "version";
    	
	/**
	 * The Component DB server related constants
	 */
	public static class DBServer {

		/**
		 * The Jdbc driver: implementation of the driver for the specific database type.
		 */
		public static final String JDBC_DRIVER = "jdbc_driver";

        /**
         * The default JDBC driver
         */
        public static final String DEFAULT_JDBC_DRIVER = "org.h2.Driver";

		/**
		 * The Jdbc URL of the database. 
		 */
		public static final String JDBC_URL = "jdbc_url";

        /**
         * Name of the H2 database file used for the default value of the JDBC URL
         */
        public static final String DEFAULT_JDBC_URL_DATABASE_FILENAME = "h2-activiti.db";

		/**
		 * The Jdbc Username : username to connect to the database. 
		 */
		public static final String JDBC_USERNAME = "jdbc_username";

		/**
		 * The Jdbc Password: password to connect to the database. 
		 */
		public static final String JDBC_PASSWORD = "jdbc_password";

        /**
         * Tag name of the component JBI descriptor about the number of active connections that the connection pool at
         * maximum at any time can contain.
         */
		public static final String JDBC_MAX_ACTIVE_CONNECTIONS = "jdbc_max_active_connections";
		
        /**
         * Default value of the number of active connections that the connection pool at maximum at any time can
         * contain.
         */
        public static final int DEFAULT_JDBC_MAX_ACTIVE_CONNECTIONS = 10;

        /**
         * Tag name of the component JBI descriptor about the number of idle connections that the connection pool at
         * maximum at any time can contain.
         */
		public static final String JDBC_MAX_IDLE_CONNECTIONS = "jdbc_max_idle_connections";
		
        /**
         * Default value of the number of idle connections that the connection pool at maximum at any time can contain.
         */
        public static final int DEFAULT_JDBC_MAX_IDLE_CONNECTIONS = 1;

        /**
         * Tag name of the component JBI descriptor about the amount of time in milliseconds a connection can be
         * 'checked out' from the connection pool before it is forcefully returned.
         */
		public static final String JDBC_MAX_CHECKOUT_TIME = "jdbc_max_checkout_time";

        /**
         * Default value of the amount of time in milliseconds a connection can be 'checked out' from the connection
         * pool before it is forcefully returned.
         */
        public static final int DEFAULT_JDBC_MAX_CHECKOUT_TIME = 20000;
		
        /**
         * Tag name of the component JBI descriptor about a low level setting that gives the pool a chance to print a
         * log status and re-attempt the acquisition of a connection in the case that it's taking unusually long (to
         * avoid failing silently forever if the pool is misconfigured).
         */
		public static final String JDBC_MAX_WAIT_TIME = "jdbc_max_wait_time";

        /**
         * Default value of {@link #JDBC_MAX_WAIT_TIME}.
         */
        public static final int DEFAULT_JDBC_MAX_WAIT_TIME = 20000;
		
		/**
		 * The databaseType: it's normally not necessary to specify this property
		 * as it is automatically analyzed from the database connection meta data.
		 * Should only be specified in case automatic detection fails.
		 * Possible values: {h2, mysql, oracle, postgres, mssql, db2}.
		 * This property is required when not using the default H2 database.
		 * This setting will determine which create/drop scripts and queries will be used.
		 * See the 'supported databases' section for an overview of which types are supported.
		 */
		public static final String DATABASE_TYPE = "database_type";
		
		/**
		 * The databaseSchemaUpdate: allows to set the strategy to handle
		 * the database schema on process engine boot and shutdown.
		 *   - false (default): Checks the version of the DB schema against the library
		 *                      when the process engine is being created
		 *                      and throws an exception if the versions don't match.
		 *   - true: Upon building the process engine, a check is performed
		 *           and an update of the schema is performed if it is necessary.
		 *           If the schema doesn't exist, it is created.
		 *   - create-drop: Creates the schema when the process engine is being created
		 *                  and drops the schema when the process engine is being closed. 
		 */
		public static final String DATABASE_SCHEMA_UPDATE = "database_schema_update";
		
	}

    private ActivitiSEConstants() {
        // Utility class => No constructor
    }
}
