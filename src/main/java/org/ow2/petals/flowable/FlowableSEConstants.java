/**
 * Copyright (c) 2014-2017 Linagora
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
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable;

import java.io.File;

import javax.xml.namespace.QName;

import org.ow2.petals.flowable.identity.file.FileIdentityService;

/**
 * The component class of the Flowable BPMN Service Engine related constants.
 * 
 * @author Bertrand Escudie - Linagora
 */
public final class FlowableSEConstants {

    /**
     * Name space name of component parameters
     */
    public static final String NAMESPACE_COMP = "http://petals.ow2.org/components/petals-se-flowable/1.0";

    /**
     * Name space name of service unit parameters
     */
    public static final String NAMESPACE_SU = "http://petals.ow2.org/components/petals-se-flowable/su/1.0";

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
        public static final String DEFAULT_JDBC_URL_DATABASE_FILENAME = "h2-flowable.db";

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

        /**
         * Default value of {@link #DATABASE_SCHEMA_UPDATE}.
         */
        public static final String DEFAULT_DATABASE_SCHEMA_UPDATE = "true";

        private DBServer() {
            // NOP: Utility class => No constructor
        }
		
	}

    /**
     * Tag name of the component JBI descriptor about the activation of the Flowable job executor.
     */
    public static final String ENGINE_ENABLE_JOB_EXECUTOR = "engine-enable-job-executor";

    /**
     * Default value of {@link #ENGINE_ENABLE_JOB_EXECUTOR}.
     */
    public static final boolean DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR = true;

    /**
     * Tag name of the component JBI descriptor about the core pool size of the Flowable job executor.
     */
    public static final String ENGINE_JOB_EXECUTOR_COREPOOLSIZE = "engine-job-executor-core-pool-size";

    /**
     * Default value of {@link #ENGINE_JOB_EXECUTOR_COREPOOLSIZE}.
     */
    public static final int DEFAULT_ENGINE_JOB_EXECUTOR_COREPOOLSIZE = 2;

    /**
     * Tag name of the component JBI descriptor about the max pool size of the Flowable job executor.
     */
    public static final String ENGINE_JOB_EXECUTOR_MAXPOOLSIZE = "engine-job-executor-max-pool-size";

    /**
     * Default value of {@link #ENGINE_JOB_EXECUTOR_MAXPOOLSIZE}.
     */
    public static final int DEFAULT_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE = 10;

    /**
     * Tag name of the component JBI descriptor about the keep alive time, in milliseconds, of idle thread of the
     * Flowable async job executor.
     */
    public static final String ENGINE_JOB_EXECUTOR_KEEPALIVETIME = "engine-job-executor-keep-alive-time";

    /**
     * Default value of {@link #ENGINE_JOB_EXECUTOR_KEEPALIVETIME}.
     */
    public static final long DEFAULT_ENGINE_JOB_EXECUTOR_KEEPALIVETIME = 5000;

    /**
     * Tag name of the component JBI descriptor about the queue size of the Flowable async job executor.
     */
    public static final String ENGINE_JOB_EXECUTOR_QUEUESIZE = "engine-job-executor-queue-size";

    /**
     * Default value of {@link #ENGINE_JOB_EXECUTOR_QUEUESIZE}.
     */
    public static final int DEFAULT_ENGINE_JOB_EXECUTOR_QUEUESIZE = 100;

    /**
     * Tag name of the component JBI descriptor about the max number of timer jobs that are fetched from the database in
     * one query by the Flowable async job executor.
     */
    public static final String ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION = "engine-job-executor-max-timer-jobs-per-acquisition";

    /**
     * Default value of {@link #ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION}.
     */
    public static final int DEFAULT_ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION = 1;

    /**
     * Tag name of the component JBI descriptor about the max number of asynchronous jobs due that are fetched from the
     * database in one query by the Flowable async job executor.
     */
    public static final String ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION = "engine-job-executor-max-async-jobs-due-per-acquisition";

    /**
     * Default value of {@link #ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION}.
     */
    public static final int DEFAULT_ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION = 1;

    /**
     * Tag name of the component JBI descriptor about the time, in milliseconds, between asynchronous jobs due queries
     * being executed by the Flowable async job executor.
     */
    public static final String ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME = "engine-job-executor-async-job-acquire-wait-time";

    /**
     * Default value of {@link #ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME}.
     */
    public static final int DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME = 10000;

    /**
     * Tag name of the component JBI descriptor about the time, in milliseconds, between timer jobs queries being
     * executed by the Flowable async job executor.
     */
    public static final String ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME = "engine-job-executor-timer-job-acquire-wait-time";

    /**
     * Default value of {@link #ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME}.
     */
    public static final int DEFAULT_ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME = 10000;

    /**
     * Tag name of the component JBI descriptor about the time, in milliseconds, that a timer job is locked before being
     * retried again by the Flowable async job executor. The Flowable engine considers the timer job to have failed
     * after this period of time and will retry.
     */
    public static final String ENGINE_JOB_EXECUTOR_TIMERLOCKTIME = "engine-job-executor-timer-lock-time";

    /**
     * Default value of {@link #ENGINE_JOB_EXECUTOR_TIMERLOCKTIME}.
     */
    public static final int DEFAULT_ENGINE_JOB_EXECUTOR_TIMERLOCKTIME = 300000;

    /**
     * Tag name of the component JBI descriptor about the time, in milliseconds, that an asynchronous timer job is
     * locked before being retried again by the Flowable async job executor. The Flowable engine considers the timer job
     * to have failed after this period of time and will retry.
     */
    public static final String ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME = "engine-job-executor-async-job-lock-time";

    /**
     * Default value of {@link #ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME}.
     */
    public static final int DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME = 300000;

    /**
     * Tag name of the component JBI descriptor about the activation of the Flowable BPMN validation during processes
     * deployment.
     */
    public static final String ENGINE_ENABLE_BPMN_VALIDATION = "engine-enable-bpmn-validation";

    /**
     * Default value of {@link #ENGINE_ENABLE_BPMN_VALIDATION}.
     */
    public static final boolean DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION = true;

    /**
     * Tag name of the component JBI descriptor about the class name of the identity service to use.
     */
    public static final String ENGINE_IDENTITY_SERVICE_CLASS_NAME = "engine-identity-service-class-name";

    /**
     * Default value of {@link #ENGINE_IDENTITY_SERVICE_CLASS_NAME}.
     */
    public static final String DEFAULT_ENGINE_IDENTITY_SERVICE_CLASS_NAME = FileIdentityService.class.getName();

    /**
     * Tag name of the component JBI descriptor about the configuration file of the identity service.
     */
    public static final String ENGINE_IDENTITY_SERVICE_CFG_FILE = "engine-identity-service-config-file";

    /**
     * Default value of {@link #ENGINE_IDENTITY_SERVICE_CFG_FILE}.
     */
    public static final File DEFAULT_ENGINE_IDENTITY_SERVICE_CFG_FILE = null;

    /**
     * Constants relatives to the Flowable components
     */
    public static class Flowable {

        /**
         * Name of the component in charge of sending message exchange coming from Petals service tasks
         */
        public static final String PETALS_SENDER_COMP_NAME = "petals-sender";

        /**
         * Name of the process variable used to store the Petals flow instance identifier into Flowable objects as
         * process instance or user tasks.
         */
        public static final String VAR_PETALS_FLOW_INSTANCE_ID = "petals.flow.instance.id";

        /**
         * Name of a variable used to store the Petals flow step identifier into Flowable objects as process instance or
         * user tasks.
         */
        public static final String VAR_PETALS_FLOW_STEP_ID = "petals.flow.step.id";

        /**
         * Name of a variable used to store the Petals flow instance identifier of the flow that has created the new
         * flow associated to the process instance.
         */
        public static final String VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID = "petals.correlated.flow.instance.id";

        /**
         * Name of the process variable used to store the Petals flow step identifier of the flow that has created the
         * new flow associated to the process instance.
         */
        public static final String VAR_PETALS_CORRELATED_FLOW_STEP_ID = "petals.correlated.flow.step.id";

        private Flowable() {
            // NOP: Utility class => No constructor
        }
    }

    public static class IntegrationOperation {

        /**
         * Name space of integration service
         */
        public static final String ITG_NAMESPACE = "http://petals.ow2.org/components/flowable/generic/1.0";

        /**
         * Interface name (local part) of the integration service 'Task'
         */
        public static final String ITG_TASK_PORT_TYPE_NAME = "Task";

        /**
         * Interface name of the integration service 'Task'
         */
        public static final QName ITG_TASK_PORT_TYPE = new QName(ITG_NAMESPACE, ITG_TASK_PORT_TYPE_NAME);

        /**
         * Name (local part) of the integration service 'Task'
         */
        public static final String ITG_TASK_SERVICE_NAME = "TaskService";

        /**
         * Name of the integration service 'Task'
         */
        public static final QName ITG_TASK_SERVICE = new QName(ITG_NAMESPACE, ITG_TASK_SERVICE_NAME);

        /**
         * Interface name (local part) of the integration service 'ProcessInstances'
         */
        public static final String ITG_PROCESSINSTANCES_PORT_TYPE_NAME = "ProcessInstances";

        /**
         * Interface name of the integration service 'ProcessInstances'
         */
        public static final QName ITG_PROCESSINSTANCES_PORT_TYPE = new QName(ITG_NAMESPACE,
                ITG_PROCESSINSTANCES_PORT_TYPE_NAME);

        /**
         * Name (local part) of the integration service 'ProcessInstances'
         */
        public static final String ITG_PROCESSINSTANCES_SERVICE_NAME = "ProcessInstancesService";

        /**
         * Name of the integration service 'Task'
         */
        public static final QName ITG_PROCESSINSTANCES_SERVICE = new QName(ITG_NAMESPACE,
                ITG_PROCESSINSTANCES_SERVICE_NAME);

        /**
         * Name (local part) of the integration operation to search tasks
         */
        public static final String ITG_OP_GETTASKS_NAME = "getTasks";

        /**
         * Name of the integration operation to search tasks
         */
        public static final QName ITG_OP_GETTASKS = new QName(ITG_NAMESPACE, ITG_OP_GETTASKS_NAME);

        /**
         * Name (local part) of the integration operation to search process instances
         */
        public static final String ITG_OP_GETPROCESSINSTANCES_NAME = "getProcessInstances";

        /**
         * Name of the integration operation to search process instances
         */
        public static final QName ITG_OP_GETPROCESSINSTANCES = new QName(ITG_NAMESPACE, ITG_OP_GETPROCESSINSTANCES_NAME);

        /**
         * Name (local part) of the integration operation to suspend process instances
         */
        public static final String ITG_OP_SUSPENDPROCESSINSTANCES_NAME = "suspendProcessInstances";

        /**
         * Name of the integration operation to suspend process instances
         */
        public static final QName ITG_OP_SUSPENDPROCESSINSTANCES = new QName(ITG_NAMESPACE,
                ITG_OP_SUSPENDPROCESSINSTANCES_NAME);

        /**
         * Name (local part) of the integration operation to activate process instances
         */
        public static final String ITG_OP_ACTIVATEPROCESSINSTANCES_NAME = "activateProcessInstances";

        /**
         * Name of the integration operation to activate process instances
         */
        public static final QName ITG_OP_ACTIVATEPROCESSINSTANCES = new QName(ITG_NAMESPACE,
                ITG_OP_ACTIVATEPROCESSINSTANCES_NAME);

        private IntegrationOperation() {
            // NOP: Utility class => No constructor
        }
    }

    private FlowableSEConstants() {
        // NOP: Utility class => No constructor
    }
}
