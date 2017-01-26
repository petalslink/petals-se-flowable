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
package org.ow2.petals.activitibpmn;

import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBDUEACQUIREWAITTIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_COREPOOLSIZE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_KEEPALIVETIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_QUEUESIZE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_TIMERLOCKTIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_ENABLE_BPMN_VALIDATION;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_ENABLE_JOB_EXECUTOR;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_IDENTITY_SERVICE_CFG_FILE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_IDENTITY_SERVICE_CLASS_NAME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBDUEACQUIREWAITTIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_JOB_EXECUTOR_COREPOOLSIZE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_JOB_EXECUTOR_KEEPALIVETIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_JOB_EXECUTOR_MAXPOOLSIZE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_JOB_EXECUTOR_QUEUESIZE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_JOB_EXECUTOR_TIMERLOCKTIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DATABASE_SCHEMA_UPDATE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DATABASE_TYPE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_DATABASE_SCHEMA_UPDATE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_JDBC_MAX_ACTIVE_CONNECTIONS;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_JDBC_MAX_CHECKOUT_TIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_JDBC_MAX_IDLE_CONNECTIONS;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_JDBC_MAX_WAIT_TIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_DRIVER;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_MAX_ACTIVE_CONNECTIONS;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_MAX_CHECKOUT_TIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_MAX_IDLE_CONNECTIONS;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_MAX_WAIT_TIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_PASSWORD;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_URL;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_USERNAME;

import java.io.File;
import java.sql.Driver;
import java.sql.DriverManager;
import java.util.Collection;
import java.util.Enumeration;

import javax.jbi.JBIException;
import javax.management.InvalidAttributeValueException;

import org.ow2.petals.activitibpmn.identity.IdentityService;
import org.ow2.petals.component.framework.DefaultBootstrap;

/**
 * The component class of the Activiti BPMN Service Engine.
 * 
 * @author Bertrand Escudie - Linagora
 */
public class ActivitiSEBootstrap extends DefaultBootstrap {

    private static final String ATTR_NAME_JDBC_DRIVER = "jdbcDriver";

    private static final String ATTR_NAME_JDBC_URL = "jdbcUrl";

    private static final String ATTR_NAME_JDBC_USERNAME = "jdbcUsername";

    private static final String ATTR_NAME_JDBC_PASSWORD = "jdbcPassword";

    private static final String ATTR_NAME_JDBC_MAX_ACTIVE_CONNECTIONS = "jdbcMaxActiveConnections";

    private static final String ATTR_NAME_JDBC_MAX_IDLE_CONNECTIONS = "jdbcMaxIdleConnections";

    private static final String ATTR_NAME_JDBC_MAX_CHECKOUT_TIME = "jdbcMaxCheckoutTime";

    private static final String ATTR_NAME_JDBC_MAX_WAIT_TIME = "jdbcMaxWaitTime";

    private static final String ATTR_NAME_DATABASE_TYPE = "databaseType";

    private static final String ATTR_NAME_DATABASE_SCHEMA_UPDATE = "databaseSchemaUpdate";

    private static final String ATTR_NAME_ENGINE_ENABLE_BPMN_VALIDATION = "engineEnableBpmnValidation";

    private static final String ATTR_NAME_ENGINE_IDENTITY_SERVICE_CLASS_NAME = "engineIdentityServiceClassName";

    private static final String ATTR_NAME_ENGINE_IDENTITY_SERVICE_CFG_FILE = "engineIdentityServiceCfgFile";

    // Parameters of the Activiti job executor
    private static final String ATTR_NAME_ENGINE_ENABLE_JOB_EXECUTOR = "engineEnableJobExecutor";

    private static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_COREPOOLSIZE = "engineJobExecutorCorePoolSize";

    private static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE = "engineJobExecutorMaxPoolSize";

    private static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_KEEPALIVETIME = "engineJobExecutorKeepAliveTime";

    private static final String ATTR_NAME_ENGINE_JOB_EXECUTOR_QUEUESIZE = "engineJobExecutorQueueSize";

    @Override
    public Collection<String> getMBeanAttributesNames() {
        this.getLogger().fine("Start ActivitiSEBootstrap.getAttributeList()");

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
            attributes.add(ATTR_NAME_ENGINE_IDENTITY_SERVICE_CLASS_NAME);
            attributes.add(ATTR_NAME_ENGINE_IDENTITY_SERVICE_CFG_FILE);

            // Parameters of the Activiti job executor
            attributes.add(ATTR_NAME_ENGINE_ENABLE_JOB_EXECUTOR);
            attributes.add(ATTR_NAME_ENGINE_JOB_EXECUTOR_COREPOOLSIZE);
            attributes.add(ATTR_NAME_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE);
            attributes.add(ATTR_NAME_ENGINE_JOB_EXECUTOR_KEEPALIVETIME);

            return attributes;
        } finally {
            this.getLogger().fine("End ActivitiSEBootstrap.getAttributeList()");
        }
    }

    /**
     * Get the jdbc Driver
     * 
     * @return the jdbc Driver
     */
    public String getJdbcDriver() {

        return ActivitiParameterReader.getJdbcDriver(this.getParam(ActivitiSEConstants.DBServer.JDBC_DRIVER),
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
        final boolean enableActivitiJobExecutor;
        final String enableActivitiJobExecutorConfigured = this.getParam(ENGINE_ENABLE_JOB_EXECUTOR);
        if (enableActivitiJobExecutorConfigured == null || enableActivitiJobExecutorConfigured.trim().isEmpty()) {
            this.getLogger().info("The activation of the Activiti job executor is not configured. Default value used.");
            enableActivitiJobExecutor = DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR;
        } else {
            enableActivitiJobExecutor = enableActivitiJobExecutorConfigured.trim().equalsIgnoreCase("false") ? false
                    : (enableActivitiJobExecutorConfigured.trim().equalsIgnoreCase("true") ? true
                            : DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR);
        }
        return Boolean.toString(enableActivitiJobExecutor);
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
     * Get the Activiti async job executor core pool size
     * 
     * @return the Activiti async job executor core pool size
     */
    public int getEngineJobExecutorCorePoolSize() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_COREPOOLSIZE, DEFAULT_ENGINE_JOB_EXECUTOR_COREPOOLSIZE);
    }

    /**
     * Set the Activiti async job executor core pool size
     * 
     * @param value
     *            the Activiti async job executor core pool size
     */
    public void setEngineJobExecutorCorePoolSize(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_COREPOOLSIZE, Integer.toString(value));
    }

    /**
     * Get the Activiti async job executor max pool size
     * 
     * @return the Activiti async job executor max pool size
     */
    public int getEngineJobExecutorMaxPoolSize() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_MAXPOOLSIZE, DEFAULT_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE);
    }

    /**
     * Set the Activiti async job executor max pool size
     * 
     * @param value
     *            the Activiti async job executor max pool size
     */
    public void setEngineJobExecutorMaxPoolSize(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_MAXPOOLSIZE, Integer.toString(value));
    }

    /**
     * Get the keep alive time of idle thread of the Activiti async job executor
     * 
     * @return the keep alive time of idle thread of the Activiti async job executor
     */
    public long getEngineJobExecutorKeepAliveTime() {
        return this.getParamAsLong(ENGINE_JOB_EXECUTOR_KEEPALIVETIME, DEFAULT_ENGINE_JOB_EXECUTOR_KEEPALIVETIME);
    }

    /**
     * Set the keep alive time of idle thread of the Activiti async job executor
     * 
     * @param value
     *            the keep alive time of idle thread of the Activiti async job executor
     */
    public void setEngineJobExecutorKeepAliveTime(final long value) {
        this.setParam(ENGINE_JOB_EXECUTOR_KEEPALIVETIME, Long.toString(value));
    }

    /**
     * Get the queue size of the Activiti async job executor
     * 
     * @return the queue size of the Activiti async job executor
     */
    public int getEngineJobExecutorQueueSize() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_QUEUESIZE, DEFAULT_ENGINE_JOB_EXECUTOR_QUEUESIZE);
    }

    /**
     * Set the queue size of the Activiti async job executor
     * 
     * @param value
     *            the queue size of the Activiti async job executor
     */
    public void setEngineJobExecutorQueueSize(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_QUEUESIZE, Integer.toString(value));
    }

    /**
     * Get the max number of timer jobs that are fetched from the database in one query by the Activiti async job
     * executor.
     * 
     * @return the max number of timer jobs that are fetched from the database in one query by the Activiti async job
     *         executor.
     */
    public int getEngineJobExecutorMaxTimerJobsPerAcquisition() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION,
                DEFAULT_ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION);
    }

    /**
     * Set the max number of timer jobs that are fetched from the database in one query by the Activiti async job
     * executor.
     * 
     * @param value
     *            the max number of timer jobs that are fetched from the database in one query by the Activiti async job
     *            executor.
     */
    public void setEngineJobExecutorMaxTimerJobsPerAcquisition(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION, Integer.toString(value));
    }

    /**
     * Get the max number of asynchronous jobs due that are fetched from the database in one query by the Activiti async
     * job executor.
     * 
     * @return the max number of asynchronous jobs due that are fetched from the database in one query by the Activiti
     *         async job executor.
     */
    public int getEngineJobExecutorMaxAsyncJobsDuePerAcquisition() {
        return this.getParamAsInteger(ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION,
                DEFAULT_ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION);
    }

    /**
     * Set the max number of asynchronous jobs due that are fetched from the database in one query by the Activiti async
     * job executor.
     * 
     * @param value
     *            the max number of asynchronous jobs due that are fetched from the database in one query by the
     *            Activiti async job executor.
     */
    public void setEngineJobExecutorMaxAsyncJobsDuePerAcquisition(final int value) {
        this.setParam(ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION, Integer.toString(value));
    }

    /**
     * Get the time, in milliseconds, between asynchronous jobs due queries being executed by the Activiti async
     * 
     * @return the time, in milliseconds, between asynchronous jobs due queries being executed by the Activiti async
     */
    public long getEngineJobExecutorAsyncJobDueAcquireWaitTime() {
        return this.getParamAsLong(ENGINE_JOB_EXECUTOR_ASYNCJOBDUEACQUIREWAITTIME,
                DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBDUEACQUIREWAITTIME);
    }

    /**
     * Set the time, in milliseconds, between asynchronous jobs due queries being executed by the Activiti async job
     * executor.
     * 
     * @param value
     *            the time, in milliseconds, between asynchronous jobs due queries being executed by the Activiti async.
     */
    public void setEngineJobExecutorAsyncJobDueAcquireWaitTime(final long value) {
        this.setParam(ENGINE_JOB_EXECUTOR_ASYNCJOBDUEACQUIREWAITTIME, Long.toString(value));
    }

    /**
     * Get the time, in milliseconds, between timer jobs queries being executed by the Activiti async job executor.
     * 
     * @return the time, in milliseconds, between timer jobs queries being executed by the Activiti async job executor.
     */
    public long getEngineJobExecutorTimerJobAcquireWaitTime() {
        return this.getParamAsLong(ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME,
                DEFAULT_ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME);
    }

    /**
     * Set the time, in milliseconds, between timer jobs queries being executed by the Activiti async job executor.
     * 
     * @param value
     *            the time, in milliseconds, between timer jobs queries being executed by the Activiti async job
     *            executor.
     */
    public void setEngineJobExecutorTimerJobAcquireWaitTime(final long value) {
        this.setParam(ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME, Long.toString(value));
    }

    /**
     * Get the time, in milliseconds, that a timer job is locked before being retried again by the Activiti async job
     * executor. The Activiti engine considers the timer job to have failed after this period of time and will retry.
     * 
     * @return the time, in milliseconds, that a timer job is locked before being retried again by the Activiti async
     *         job executor.
     */
    public long getEngineJobExecutorTimerLockTime() {
        return this.getParamAsLong(ENGINE_JOB_EXECUTOR_TIMERLOCKTIME, DEFAULT_ENGINE_JOB_EXECUTOR_TIMERLOCKTIME);
    }

    /**
     * Set the time, in milliseconds, that a timer job is locked before being retried again by the Activiti async job
     * executor. The Activiti engine considers the timer job to have failed after this period of time and will retry.
     * 
     * @param value
     *            the time, in milliseconds, that a timer job is locked before being retried again by the Activiti async
     *            job executor.
     */
    public void setEngineJobExecutorTimerLockTime(final long value) {
        this.setParam(ENGINE_JOB_EXECUTOR_TIMERLOCKTIME, Long.toString(value));
    }

    /**
     * Get the time, in milliseconds, that an asynchronous job is locked before being retried again by the Activiti
     * async job executor. The Activiti engine considers the timer job to have failed after this period of time and will
     * retry.
     * 
     * @return the time, in milliseconds, that an asynchronous job is locked before being retried again by the Activiti
     *         async job executor.
     */
    public long getEngineJobExecutorAsyncJobLockTime() {
        return this.getParamAsLong(ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME, DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME);
    }

    /**
     * Set the time, in milliseconds, that an asynchronous job is locked before being retried again by the Activiti
     * async job executor. The Activiti engine considers the timer job to have failed after this period of time and will
     * retry.
     * 
     * @param value
     *            the time, in milliseconds, that an asynchronous job is locked before being retried again by the
     *            Activiti async job executor.
     */
    public void setEngineJobExecutorAsyncJobLockTime(final long value) {
        this.setParam(ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME, Long.toString(value));
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
        final boolean enableActivitiBpmnValidation;
        final String enableActivitiBpmnValidationConfigured = this.getParam(ENGINE_ENABLE_BPMN_VALIDATION);
        if (enableActivitiBpmnValidationConfigured == null || enableActivitiBpmnValidationConfigured.trim().isEmpty()) {
            this.getLogger().info(
                    "The activation of the BPMN validation on process deployments into Activiti engine is not configured. Default value used.");
            enableActivitiBpmnValidation = DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION;
        } else {
            enableActivitiBpmnValidation = enableActivitiBpmnValidationConfigured.trim().equalsIgnoreCase("false")
                    ? false
                    : (enableActivitiBpmnValidationConfigured.trim().equalsIgnoreCase("true") ? true
                            : DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION);
        }
        return Boolean.toString(enableActivitiBpmnValidation);
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
     * Get the identity service class name
     * 
     * @return the identity service class name
     */
    public String getEngineIdentityServiceClassName() {

        try {
            return ActivitiParameterReader.getEngineIdentityServiceClassName(
                    this.getParam(ENGINE_IDENTITY_SERVICE_CLASS_NAME), this.getLogger()).getName();
        } catch (final JBIException e) {
            return ActivitiSEConstants.DEFAULT_ENGINE_IDENTITY_SERVICE_CLASS_NAME;
        }
    }

    /**
     * Set the identity service class name
     * 
     * @param value
     *            the identity service class name. Must be a loadable class implementing {@link IdentityService}
     * @throws InvalidAttributeValueException
     */
    public void setEngineIdentityServiceClassName(final String value) throws InvalidAttributeValueException {

        if (value == null || value.trim().isEmpty()) {
            // No identity service configured
            this.setParam(ENGINE_IDENTITY_SERVICE_CLASS_NAME, null);
        } else {
            this.setParamAsImplementationClassURI(ENGINE_IDENTITY_SERVICE_CLASS_NAME, value, IdentityService.class);
        }
    }

    /**
     * Get the identity service configuration file
     * 
     * @return the identity service configuration file
     */
    public String getEngineIdentityServiceCfgFile() {

        final File confFile = ActivitiParameterReader.getEngineIdentityServiceConfigurationFile(
                this.getParam(ENGINE_IDENTITY_SERVICE_CFG_FILE), this.getLogger());
        return confFile == null ? null : confFile.getAbsolutePath();
    }

    /**
     * Set the identity service configuration file
     * 
     * @param value
     *            the identity service configuration file
     */
    public void setEngineIdentityServiceCfgFile(final String value) throws InvalidAttributeValueException {

        if (value == null || value.trim().isEmpty()) {
            // No identity service configuration file configured
            this.setParam(ENGINE_IDENTITY_SERVICE_CFG_FILE, null);
        } else {
            final File tmpFile = new File(value.trim());
            if (!tmpFile.isAbsolute()) {
                // The identity service configuration file is a resource

                // TODO: Add a check to verify that the resource
                this.setParam(ENGINE_IDENTITY_SERVICE_CFG_FILE, value.trim());
            } else if (!tmpFile.exists()) {
                throw new InvalidAttributeValueException(
                        "The identity service configuration file (" + value.trim() + ") does not exist.");
            } else if (!tmpFile.isFile()) {
                throw new InvalidAttributeValueException(
                        "The identity service configuration file (" + value.trim() + ") is not a file.");
            } else {
                this.setParam(ENGINE_IDENTITY_SERVICE_CFG_FILE, value.trim());
            }
        }

    }

}
