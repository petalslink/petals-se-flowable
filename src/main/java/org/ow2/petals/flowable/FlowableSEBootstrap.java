/**
 * Copyright (c) 2014-2020 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ASYNC_FAILED_JOB_WAIT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_DEFAULT_FAILED_JOB_WAIT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_COREPOOLSIZE;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_KEEPALIVETIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_QUEUESIZE;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_TIMERLOCKTIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_REST_API_ACCESS_PRIVILEGE;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_REST_API_ADDRESS;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_REST_API_ENABLE;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_REST_API_PORT;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_ASYNC_FAILED_JOB_WAIT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_DEFAULT_FAILED_JOB_WAIT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_ENABLE_BPMN_VALIDATION;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_JOB_EXECUTOR_COREPOOLSIZE;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_JOB_EXECUTOR_KEEPALIVETIME;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXPOOLSIZE;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_JOB_EXECUTOR_QUEUESIZE;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERLOCKTIME;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_REST_API_ACCESS_PRIVILEGE;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_REST_API_ADDRESS;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_REST_API_ENABLE;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_REST_API_PORT;
import static org.ow2.petals.flowable.FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CFG_FILE;
import static org.ow2.petals.flowable.FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CLASS_NAME;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DATABASE_SCHEMA_UPDATE;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DATABASE_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_DATABASE_SCHEMA_UPDATE;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_JDBC_MAX_ACTIVE_CONNECTIONS;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_JDBC_MAX_CHECKOUT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_JDBC_MAX_IDLE_CONNECTIONS;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_JDBC_MAX_WAIT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_DRIVER;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_MAX_ACTIVE_CONNECTIONS;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_MAX_CHECKOUT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_MAX_IDLE_CONNECTIONS;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_MAX_WAIT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_PASSWORD;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_URL;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_USERNAME;

import java.io.File;
import java.sql.Driver;
import java.sql.DriverManager;
import java.util.Collection;
import java.util.Enumeration;

import javax.jbi.JBIException;
import javax.management.InvalidAttributeValueException;

import org.ow2.petals.component.framework.DefaultBootstrap;
import org.ow2.petals.flowable.identity.SeFlowableIdmEngineConfigurator;

/**
 * The component class of the Flowable BPMN Service Engine.
 * 
 * @author Bertrand Escudie - Linagora
 */
public class FlowableSEBootstrap extends DefaultBootstrap {

    public static final String ATTR_NAME_JDBC_DRIVER = "jdbcDriver";

    public static final String ATTR_NAME_JDBC_URL = "jdbcUrl";

    public static final String ATTR_NAME_JDBC_USERNAME = "jdbcUsername";

    public static final String ATTR_NAME_JDBC_PASSWORD = "jdbcPassword";

    public static final String ATTR_NAME_JDBC_MAX_ACTIVE_CONNECTIONS = "jdbcMaxActiveConnections";

    public static final String ATTR_NAME_JDBC_MAX_IDLE_CONNECTIONS = "jdbcMaxIdleConnections";

    public static final String ATTR_NAME_JDBC_MAX_CHECKOUT_TIME = "jdbcMaxCheckoutTime";

    public static final String ATTR_NAME_JDBC_MAX_WAIT_TIME = "jdbcMaxWaitTime";

    public static final String ATTR_NAME_DATABASE_TYPE = "databaseType";

    public static final String ATTR_NAME_DATABASE_SCHEMA_UPDATE = "databaseSchemaUpdate";

    public static final String ATTR_NAME_ENGINE_ENABLE_BPMN_VALIDATION = "engineEnableBpmnValidation";

    public static final String ATTR_NAME_ENGINE_ASYNC_FAILED_JOB_WAITTIME = "engineAsyncFailedJobWaitTime";

    public static final String ATTR_NAME_ENGINE_DEFAULT_FAILED_JOB_WAITTIME = "engineDefaultFailedJobWaitTime";

    public static final String ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CLASS_NAME = "idmEngineConfiguratorClassName";

    public static final String ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CFG_FILE = "idmEngineConfiguratorConfigFile";

    // Parameters of the Flowable job executor
    public static final String ATTR_NAME_ENGINE_ENABLE_JOB_EXECUTOR = "engineEnableJobExecutor";

    public static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_COREPOOLSIZE = "engineJobExecutorCorePoolSize";

    public static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE = "engineJobExecutorMaxPoolSize";

    public static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_KEEPALIVETIME = "engineJobExecutorKeepAliveTime";

    public static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_QUEUESIZE = "engineJobExecutorQueueSize";

    public static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION = "engineJobExecutorMaxTimerJobsPerAcquisition";

    public static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION = "engineJobExecutorMaxAsyncJobsDuePerAcquisition";

    public static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME = "engineJobExecutorAsyncJobAcquireWaitTime";

    public static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME = "engineJobExecutorTimerJobAcquireWaitTime";

    public static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_TIMERLOCKTIME = "engineJobExecutorTimerLockTime";

    public static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME = "engineJobExecutorAsyncJobLockTime";

    public static final String ATTR_NAME_ENGINE_REST_API_ENABLE = "engineRestApiEnable";

    public static final String ATTR_NAME_ENGINE_REST_API_ADDRESS = "engineRestApiAddress";

    public static final String ATTR_NAME_ENGINE_REST_API_PORT = "engineRestApiPort";

    public static final String ATTR_NAME_ENGINE_REST_API_ACCESS_PRIVILEGE = "engineRestApiAccessPrivilege";

    @Override
    public Collection<String> getMBeanAttributesNames() {
        this.getLogger().fine("Start FlowableSEBootstrap.getAttributeList()");

        try {
            final Collection<String> attributes = super.getMBeanAttributesNames();

            attributes.add(ATTR_NAME_JDBC_DRIVER);
            attributes.add(ATTR_NAME_JDBC_URL);
            attributes.add(ATTR_NAME_JDBC_USERNAME);
            attributes.add(ATTR_NAME_JDBC_PASSWORD);
            attributes.add(ATTR_NAME_JDBC_MAX_ACTIVE_CONNECTIONS);
            attributes.add(ATTR_NAME_JDBC_MAX_IDLE_CONNECTIONS);
            attributes.add(ATTR_NAME_JDBC_MAX_CHECKOUT_TIME);
            attributes.add(ATTR_NAME_JDBC_MAX_WAIT_TIME);
            attributes.add(ATTR_NAME_DATABASE_TYPE);
            attributes.add(ATTR_NAME_DATABASE_SCHEMA_UPDATE);
            attributes.add(ATTR_NAME_ENGINE_ENABLE_BPMN_VALIDATION);
            attributes.add(ATTR_NAME_ENGINE_ASYNC_FAILED_JOB_WAITTIME);
            attributes.add(ATTR_NAME_ENGINE_DEFAULT_FAILED_JOB_WAITTIME);
            attributes.add(ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CLASS_NAME);
            attributes.add(ATTR_NAME_IDM_ENGINE_CONFIGURATOR_CFG_FILE);

            // Parameters of the Flowable job executor
            attributes.add(ATTR_NAME_ENGINE_ENABLE_JOB_EXECUTOR);
            attributes.add(ATTR_NAME_ENGINE_JOB_EXECUTOR_COREPOOLSIZE);
            attributes.add(ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE);
            attributes.add(ATTR_NAME_ENGINE_JOB_EXECUTOR_KEEPALIVETIME);
            attributes.add(ATTR_NAME_ENGINE_JOB_EXECUTOR_QUEUESIZE);
            attributes.add(ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION);
            attributes.add(ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION);
            attributes.add(ATTR_NAME_ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME);
            attributes.add(ATTR_NAME_ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME);
            attributes.add(ATTR_NAME_ENGINE_JOB_EXECUTOR_TIMERLOCKTIME);
            attributes.add(ATTR_NAME_ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME);

            // Parameters of the Floable REST API
            attributes.add(ATTR_NAME_ENGINE_REST_API_ENABLE);
            attributes.add(ATTR_NAME_ENGINE_REST_API_ADDRESS);
            attributes.add(ATTR_NAME_ENGINE_REST_API_PORT);
            attributes.add(ATTR_NAME_ENGINE_REST_API_ACCESS_PRIVILEGE);

            return attributes;
        } finally {
            this.getLogger().fine("End FlowableSEBootstrap.getAttributeList()");
        }
    }

    /**
     * Get the jdbc Driver
     * 
     * @return the jdbc Driver
     */
    public String getJdbcDriver() {

        return FlowableParameterReader.getJdbcDriver(this.getParam(FlowableSEConstants.DBServer.JDBC_DRIVER),
                this.getLogger());
    }

    /**
     * Set the jdbc Driver
     * 
     * @param value
     *            the jdbc Driver
     * @throws InvalidAttributeValueException
     */
    public void setJdbcDriver(final String value) throws InvalidAttributeValueException {

        if (value != null && !value.trim().isEmpty()) {
            // Check that the given value is a JDBC Driver
            try {
                Class.forName(value);
                boolean isRightValue = false;
                final Enumeration<Driver> drivers = DriverManager.getDrivers();
                while (drivers.hasMoreElements()) {
                    if (drivers.nextElement().getClass().getName().equals(value)) {
                        isRightValue = true;
                        break;
                    }
                }
                if (!isRightValue) {
                    throw new InvalidAttributeValueException("Invalid value for attribute '" + ATTR_NAME_JDBC_DRIVER
                            + "': " + value + " is not known as JDBC Driver.");
                } else {
                    this.setParam(JDBC_DRIVER, value);
                }
            } catch (final ClassNotFoundException e) {
                throw new InvalidAttributeValueException("Invalid value for attribute '" + ATTR_NAME_JDBC_DRIVER
                        + "': The class '" + value + "' has not been found.");
            }
        } else {
            this.setParam(JDBC_DRIVER, null);
        }

    }

    /**
     * Get the jdbc URL
     * 
     * @return the jdbc URL
     */
    public String getJdbcUrl() {
        return this.getParam(JDBC_URL);
    }

    /**
     * Set the jdbc URL
     * 
     * @param value
     *            the jdbc URL
     */
    public void setJdbcUrl(final String value) throws InvalidAttributeValueException {

        if (value != null && !value.trim().isEmpty()) {
            // Check that the given value is an URI: A JDBC is an URI, not an URL
            this.setParamAsURI(JDBC_URL, value);
        } else {
            this.setParam(JDBC_URL, null);
        }
    }

    /**
     * Get the jdbc User Name
     * 
     * @return the jdbc User Name
     */
    public String getJdbcUsername() {
        return this.getParam(JDBC_USERNAME);
    }

    /**
     * Set the jdbc User Name
     * 
     * @param value
     *            the User Name
     */
    public void setJdbcUsername(final String value) {
        this.setParam(JDBC_USERNAME, value);
    }

    /**
     * Get the jdbc Password
     * 
     * @return the jdbc Password
     */
    public String getJdbcPassword() {
        return this.getParam(JDBC_PASSWORD);
    }

    /**
     * Set the jdbc UserName
     * 
     * @param value
     *            the UserName
     */
    public void setJdbcPassword(final String value) {
        this.setParam(JDBC_PASSWORD, value);
    }

    /**
     * Get the jdbc MaxActiveConnections
     * 
     * @return the jdbc MaxActiveConnections
     */
    public int getJdbcMaxActiveConnections() {
        return this.getParamAsInteger(JDBC_MAX_ACTIVE_CONNECTIONS, DEFAULT_JDBC_MAX_ACTIVE_CONNECTIONS);
    }

    /**
     * Set the jdbc MaxActiveConnections
     * 
     * @param value
     *            the MaxActiveConnections
     */
    public void setJdbcMaxActiveConnections(final int value) {
        this.setParam(JDBC_MAX_ACTIVE_CONNECTIONS, Integer.toString(value));
    }

    /**
     * Get the jdbc MaxIdleConnections
     * 
     * @return the jdbc MaxIdleConnections
     */
    public int getJdbcMaxIdleConnections() {
        return this.getParamAsInteger(JDBC_MAX_IDLE_CONNECTIONS, DEFAULT_JDBC_MAX_IDLE_CONNECTIONS);
    }

    /**
     * Set the jdbc MaxIdleConnections
     * 
     * @param value
     *            the MaxIdleConnections
     */
    public void setJdbcMaxIdleConnections(final int value) {
        this.setParam(JDBC_MAX_IDLE_CONNECTIONS, Integer.toString(value));
    }

    /**
     * Get the jdbc MaxCheckoutTime
     * 
     * @return the jdbc MaxCheckoutTime
     */
    public int getJdbcMaxCheckoutTime() {
        return this.getParamAsInteger(JDBC_MAX_CHECKOUT_TIME, DEFAULT_JDBC_MAX_CHECKOUT_TIME);
    }

    /**
     * Set the jdbc MaxCheckoutTime
     * 
     * @param value
     *            the MaxCheckoutTime
     */
    public void setJdbcMaxCheckoutTime(final int value) {
        this.setParam(JDBC_MAX_CHECKOUT_TIME, Integer.toString(value));
    }

    /**
     * Get the jdbc MaxWaitTime
     * 
     * @return the jdbc MaxWaitTime
     */
    public int getJdbcMaxWaitTime() {
        return this.getParamAsInteger(JDBC_MAX_WAIT_TIME, DEFAULT_JDBC_MAX_WAIT_TIME);
    }

    /**
     * Set the jdbc MaxWaitTime
     * 
     * @param value
     *            the MaxWaitTime
     */
    public void setJdbcMaxWaitTime(final int value) {
        this.setParam(JDBC_MAX_WAIT_TIME, Integer.toString(value));
    }

    /**
     * Get the databaseType
     * 
     * @return the databaseType
     */
    public String getDatabaseType() {
        return this.getParam(DATABASE_TYPE);
    }

    /**
     * Set the databaseType
     * 
     * @param value
     *            the databaseType
     */
    public void setDatabaseType(final String value) {
        // TODO: Add a check about valid values
        this.setParam(DATABASE_TYPE, value);
    }

    /**
     * Get the databaseSchemaUpdate
     * 
     * @return the databaseSchemaUpdate
     */
    public String getDatabaseSchemaUpdate() {

        final String databaseSchemaUpdate;
        final String databaseSchemaUpdateString = this.getParam(DATABASE_SCHEMA_UPDATE);
        if (databaseSchemaUpdateString == null || databaseSchemaUpdateString.trim().isEmpty()) {
            databaseSchemaUpdate = DEFAULT_DATABASE_SCHEMA_UPDATE;
        } else if (databaseSchemaUpdateString.trim().equals("false") || databaseSchemaUpdateString.trim().equals("true")
                || databaseSchemaUpdateString.trim().equals("create-drop")) {
            databaseSchemaUpdate = databaseSchemaUpdateString.trim();
        } else {
            databaseSchemaUpdate = DEFAULT_DATABASE_SCHEMA_UPDATE;
        }

        return databaseSchemaUpdate;
    }

    /**
     * Set the databaseSchemaUpdate
     * 
     * @param value
     *            the databaseSchemaUpdate
     */
    public void setDatabaseSchemaUpdate(final String value) {
        // TODO: Add a check about valid values
        this.setParam(DATABASE_SCHEMA_UPDATE, value);
    }

    /**
     * Get the engineEnableJobExecutor
     * 
     * @return the engineEnableJobExecutor
     */
    public String getEngineEnableJobExecutor() {

        // Caution:
        // - only the value "false", ignoring case and spaces will disable the job executor,
        // - only the value "true", ignoring case and spaces will enable the job executor,
        // - otherwise, the default value is used.
        final boolean enableFlowableJobExecutor;
        final String enableFlowableJobExecutorConfigured = this.getParam(ENGINE_ENABLE_JOB_EXECUTOR);
        if (enableFlowableJobExecutorConfigured == null || enableFlowableJobExecutorConfigured.trim().isEmpty()) {
            this.getLogger().info("The activation of the Flowable job executor is not configured. Default value used.");
            enableFlowableJobExecutor = DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR;
        } else {
            enableFlowableJobExecutor = enableFlowableJobExecutorConfigured.trim().equalsIgnoreCase("false") ? false
                    : (enableFlowableJobExecutorConfigured.trim().equalsIgnoreCase("true") ? true
                            : DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR);
        }
        return Boolean.toString(enableFlowableJobExecutor);
    }

    /**
     * Set the engineEnableJobExecutor
     * 
     * @param value
     *            the engineEnableJobExecutor
     */
    public void setEngineEnableJobExecutor(final String value) {
        // TODO: Add a check about valid values
        this.setParam(ENGINE_ENABLE_JOB_EXECUTOR, value);
    }

    /**
     * Get the Flowable async job executor core pool size
     * 
     * @return the Flowable async job executor core pool size
     */
    public int getEngineJobExecutorCorePoolSize() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_COREPOOLSIZE, DEFAULT_ENGINE_JOB_EXECUTOR_COREPOOLSIZE);
    }

    /**
     * Set the Flowable async job executor core pool size
     * 
     * @param value
     *            the Flowable async job executor core pool size
     */
    public void setEngineJobExecutorCorePoolSize(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_COREPOOLSIZE, Integer.toString(value));
    }

    /**
     * Get the Flowable async job executor max pool size
     * 
     * @return the Flowable async job executor max pool size
     */
    public int getEngineJobExecutorMaxPoolSize() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_MAXPOOLSIZE, DEFAULT_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE);
    }

    /**
     * Set the Flowable async job executor max pool size
     * 
     * @param value
     *            the Flowable async job executor max pool size
     */
    public void setEngineJobExecutorMaxPoolSize(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_MAXPOOLSIZE, Integer.toString(value));
    }

    /**
     * Get the keep alive time of idle thread of the Flowable async job executor
     * 
     * @return the keep alive time of idle thread of the Flowable async job executor
     */
    public long getEngineJobExecutorKeepAliveTime() {
        return this.getParamAsLong(ENGINE_JOB_EXECUTOR_KEEPALIVETIME, DEFAULT_ENGINE_JOB_EXECUTOR_KEEPALIVETIME);
    }

    /**
     * Set the keep alive time of idle thread of the Flowable async job executor
     * 
     * @param value
     *            the keep alive time of idle thread of the Flowable async job executor
     */
    public void setEngineJobExecutorKeepAliveTime(final long value) {
        this.setParam(ENGINE_JOB_EXECUTOR_KEEPALIVETIME, Long.toString(value));
    }

    /**
     * Get the queue size of the Flowable async job executor
     * 
     * @return the queue size of the Flowable async job executor
     */
    public int getEngineJobExecutorQueueSize() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_QUEUESIZE, DEFAULT_ENGINE_JOB_EXECUTOR_QUEUESIZE);
    }

    /**
     * Set the queue size of the Flowable async job executor
     * 
     * @param value
     *            the queue size of the Flowable async job executor
     */
    public void setEngineJobExecutorQueueSize(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_QUEUESIZE, Integer.toString(value));
    }

    /**
     * Get the max number of timer jobs that are fetched from the database in one query by the Flowable async job
     * executor.
     * 
     * @return the max number of timer jobs that are fetched from the database in one query by the Flowable async job
     *         executor.
     */
    public int getEngineJobExecutorMaxTimerJobsPerAcquisition() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION,
                DEFAULT_ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION);
    }

    /**
     * Set the max number of timer jobs that are fetched from the database in one query by the Flowable async job
     * executor.
     * 
     * @param value
     *            the max number of timer jobs that are fetched from the database in one query by the Flowable async job
     *            executor.
     */
    public void setEngineJobExecutorMaxTimerJobsPerAcquisition(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION, Integer.toString(value));
    }

    /**
     * Get the max number of asynchronous jobs due that are fetched from the database in one query by the Flowable async
     * job executor.
     * 
     * @return the max number of asynchronous jobs due that are fetched from the database in one query by the Flowable
     *         async job executor.
     */
    public int getEngineJobExecutorMaxAsyncJobsDuePerAcquisition() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION,
                DEFAULT_ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION);
    }

    /**
     * Set the max number of asynchronous jobs due that are fetched from the database in one query by the Flowable async
     * job executor.
     * 
     * @param value
     *            the max number of asynchronous jobs due that are fetched from the database in one query by the
     *            Flowable async job executor.
     */
    public void setEngineJobExecutorMaxAsyncJobsDuePerAcquisition(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION, Integer.toString(value));
    }

    /**
     * Get the time, in milliseconds, between asynchronous jobs due queries being executed by the Flowable async
     * 
     * @return the time, in milliseconds, between asynchronous jobs due queries being executed by the Flowable async
     */
    public int getEngineJobExecutorAsyncJobAcquireWaitTime() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME,
                DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME);
    }

    /**
     * Set the time, in milliseconds, between asynchronous jobs due queries being executed by the Flowable async job
     * executor.
     * 
     * @param value
     *            the time, in milliseconds, between asynchronous jobs due queries being executed by the Flowable async.
     */
    public void setEngineJobExecutorAsyncJobAcquireWaitTime(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME, Integer.toString(value));
    }

    /**
     * Get the time, in milliseconds, between timer jobs queries being executed by the Flowable async job executor.
     * 
     * @return the time, in milliseconds, between timer jobs queries being executed by the Flowable async job executor.
     */
    public int getEngineJobExecutorTimerJobAcquireWaitTime() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME,
                DEFAULT_ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME);
    }

    /**
     * Set the time, in milliseconds, between timer jobs queries being executed by the Flowable async job executor.
     * 
     * @param value
     *            the time, in milliseconds, between timer jobs queries being executed by the Flowable async job
     *            executor.
     */
    public void setEngineJobExecutorTimerJobAcquireWaitTime(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME, Integer.toString(value));
    }

    /**
     * Get the time, in milliseconds, that a timer job is locked before being retried again by the Flowable async job
     * executor. The Flowable engine considers the timer job to have failed after this period of time and will retry.
     * 
     * @return the time, in milliseconds, that a timer job is locked before being retried again by the Flowable async
     *         job executor.
     */
    public int getEngineJobExecutorTimerLockTime() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_TIMERLOCKTIME, DEFAULT_ENGINE_JOB_EXECUTOR_TIMERLOCKTIME);
    }

    /**
     * Set the time, in milliseconds, that a timer job is locked before being retried again by the Flowable async job
     * executor. The Flowable engine considers the timer job to have failed after this period of time and will retry.
     * 
     * @param value
     *            the time, in milliseconds, that a timer job is locked before being retried again by the Flowable async
     *            job executor.
     */
    public void setEngineJobExecutorTimerLockTime(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_TIMERLOCKTIME, Integer.toString(value));
    }

    /**
     * Get the time, in milliseconds, that an asynchronous job is locked before being retried again by the Flowable
     * async job executor. The Flowable engine considers the timer job to have failed after this period of time and will
     * retry.
     * 
     * @return the time, in milliseconds, that an asynchronous job is locked before being retried again by the Flowable
     *         async job executor.
     */
    public int getEngineJobExecutorAsyncJobLockTime() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME,
                DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME);
    }

    /**
     * Set the time, in milliseconds, that an asynchronous job is locked before being retried again by the Flowable
     * async job executor. The Flowable engine considers the timer job to have failed after this period of time and will
     * retry.
     * 
     * @param value
     *            the time, in milliseconds, that an asynchronous job is locked before being retried again by the
     *            Flowable async job executor.
     */
    public void setEngineJobExecutorAsyncJobLockTime(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME, Integer.toString(value));
    }

    /**
     * Get the engineEnableBpmnValidation
     * 
     * @return the engineEnableBpmnValidation
     */
    public String getEngineEnableBpmnValidation() {

        // Caution:
        // - only the value "false", ignoring case and spaces will disable the BPMN validation,
        // - only the value "true", ignoring case and spaces will enable the BPMN validation,
        // - otherwise, the default value is used.
        final boolean enableFlowableBpmnValidation;
        final String enableFlowableBpmnValidationConfigured = this.getParam(ENGINE_ENABLE_BPMN_VALIDATION);
        if (enableFlowableBpmnValidationConfigured == null || enableFlowableBpmnValidationConfigured.trim().isEmpty()) {
            this.getLogger().info(
                    "The activation of the BPMN validation on process deployments into Flowable engine is not configured. Default value used.");
            enableFlowableBpmnValidation = DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION;
        } else {
            enableFlowableBpmnValidation = enableFlowableBpmnValidationConfigured.trim().equalsIgnoreCase("false")
                    ? false
                    : (enableFlowableBpmnValidationConfigured.trim().equalsIgnoreCase("true") ? true
                            : DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION);
        }
        return Boolean.toString(enableFlowableBpmnValidation);
    }

    /**
     * Set the engineEnableBpmnValidation
     * 
     * @param value
     *            the engineEnableBpmnValidation
     */
    public void setEngineEnableBpmnValidation(final String value) {
        // TODO: Add a check about valid values
        this.setParam(ENGINE_ENABLE_BPMN_VALIDATION, value);
    }

    /**
     * Get the default failed job wait time
     * 
     * @return the default failed job wait time
     */
    public int getEngineDefaultFailedJobWaitTime() {
        return this.getParamAsInteger(ENGINE_DEFAULT_FAILED_JOB_WAIT_TIME, DEFAULT_DEFAULT_FAILED_JOB_WAIT_TIME);
    }

    /**
     * Set the default failed job wait time
     * 
     * @param value
     *            the default failed job wait time
     */
    public void setEngineDefaultFailedJobWaitTime(final int value) {
        this.setParam(ENGINE_DEFAULT_FAILED_JOB_WAIT_TIME, Integer.toString(value));
    }

    /**
     * Get the async failed job wait time
     * 
     * @return the async failed job wait time
     */
    public int getEngineAsyncFailedJobWaitTime() {
        return this.getParamAsInteger(ENGINE_ASYNC_FAILED_JOB_WAIT_TIME, DEFAULT_ASYNC_FAILED_JOB_WAIT_TIME);
    }

    /**
     * Set the async failed job wait time
     * 
     * @param value
     *            the async failed job wait time
     */
    public void setEngineAsyncFailedJobWaitTime(final int value) {
        this.setParam(ENGINE_ASYNC_FAILED_JOB_WAIT_TIME, Integer.toString(value));
    }

    /**
     * Get the IDM engine configurator class name
     * 
     * @return the IDM engine configurator class name
     */
    public String getIdmEngineConfiguratorClassName() {

        try {
            return FlowableParameterReader.getIdmEngineConfiguratorClassName(
                    this.getParam(IDM_ENGINE_CONFIGURATOR_CLASS_NAME), this.getLogger()).getName();
        } catch (final JBIException e) {
            return FlowableSEConstants.DEFAULT_IDM_ENGINE_CONFIGURATOR_CLASS_NAME;
        }
    }

    /**
     * Set the IDM engine configurator class name
     * 
     * @param value
     *            the IDM engine configurator class name. Must be a loadable class implementing
     *            {@link SeFlowableIdmEngineConfigurator}
     * @throws InvalidAttributeValueException
     */
    public void setIdmEngineConfiguratorClassName(final String value) throws InvalidAttributeValueException {

        if (value == null || value.trim().isEmpty()) {
            // No IDM engine configurator configured
            this.setParam(IDM_ENGINE_CONFIGURATOR_CLASS_NAME, null);
        } else {
            this.setParamAsImplementationClassURI(IDM_ENGINE_CONFIGURATOR_CLASS_NAME, value,
                    SeFlowableIdmEngineConfigurator.class);
        }
    }

    /**
     * Get the IDM engine configurator configuration file
     * 
     * @return the IDM engine configurator configuration file
     */
    public String getIdmEngineConfiguratorConfigFile() {

        final File confFile = FlowableParameterReader.getEngineIdentityServiceConfigurationFile(
                this.getParam(IDM_ENGINE_CONFIGURATOR_CFG_FILE), this.getLogger());
        return confFile == null ? null : confFile.getAbsolutePath();
    }

    /**
     * Set the IDM engine configurator configuration file
     * 
     * @param value
     *            the IDM engine configurator configuration file
     */
    public void setIdmEngineConfiguratorConfigFile(final String value) throws InvalidAttributeValueException {

        if (value == null || value.trim().isEmpty()) {
            // No IDM engine configurator configuration file configured
            this.setParam(IDM_ENGINE_CONFIGURATOR_CFG_FILE, null);
        } else {
            final File tmpFile = new File(value.trim());
            if (!tmpFile.isAbsolute()) {
                // The IDM engine configurator configuration file is a resource

                // TODO: Add a check to verify that the resource
                this.setParam(IDM_ENGINE_CONFIGURATOR_CFG_FILE, value.trim());
            } else if (!tmpFile.exists()) {
                throw new InvalidAttributeValueException(
                        "The IDM engine configurator configuration file (" + value.trim() + ") does not exist.");
            } else if (!tmpFile.isFile()) {
                throw new InvalidAttributeValueException(
                        "The IDM engine configurator configuration file (" + value.trim() + ") is not a file.");
            } else {
                this.setParam(IDM_ENGINE_CONFIGURATOR_CFG_FILE, value.trim());
            }
        }

    }

    public boolean getEngineRestApiEnable() {

        // Caution:
        // - only the value "false", ignoring case and spaces will disable the rest api,
        // - only the value "true", ignoring case and spaces will enable the rest api,
        // - otherwise, the default value is used.
        final boolean engineRestApiEnable;
        final String configuredEngineRestApiEnable = this.getParam(ENGINE_REST_API_ENABLE);
        if (configuredEngineRestApiEnable == null || configuredEngineRestApiEnable.trim().isEmpty()) {
            this.getLogger().info("The activation of the Flowable job executor is not configured. Default value used.");
            engineRestApiEnable = DEFAULT_ENGINE_REST_API_ENABLE;
        } else {
            engineRestApiEnable = configuredEngineRestApiEnable.trim().equalsIgnoreCase("false") ? false
                    : (configuredEngineRestApiEnable.trim().equalsIgnoreCase("true") ? true
                            : DEFAULT_ENGINE_REST_API_ENABLE);
        }
        return engineRestApiEnable;
    }

    public void setEngineRestApiEnable(final boolean value) {
        this.setParam(ENGINE_REST_API_ENABLE, Boolean.toString(value));
    }

    public String getEngineRestApiAddress() {
        return this.getParamAsString(ENGINE_REST_API_ADDRESS, DEFAULT_ENGINE_REST_API_ADDRESS);
    }

    public void setEngineRestApiAddress(final String value) {
        this.setParam(ENGINE_REST_API_ADDRESS, value);
    }

    public int getEngineRestApiPort() {
        return this.getParamAsInteger(ENGINE_REST_API_PORT, DEFAULT_ENGINE_REST_API_PORT);
    }

    public void setEngineRestApiPort(final int value) throws InvalidAttributeValueException {
        if (value <= 0 || value > 65535) {
            throw new InvalidAttributeValueException(
                    "The value for the parameter " + ENGINE_REST_API_PORT + " is not a valid TCP port");
        }
        this.setParamAsPositiveInteger(ENGINE_REST_API_PORT, value);
    }

    public String getEngineRestApiAccessPrivilege() {
        return this.getParamAsString(ENGINE_REST_API_ACCESS_PRIVILEGE, DEFAULT_ENGINE_REST_API_ACCESS_PRIVILEGE);
    }

    public void setEngineRestApiAccessPrivilege(final String value) {
        this.setParam(ENGINE_REST_API_ACCESS_PRIVILEGE, value);
    }
}
