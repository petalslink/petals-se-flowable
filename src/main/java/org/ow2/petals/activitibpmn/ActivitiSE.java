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
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_ENABLE_BPMN_VALIDATION;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_ENABLE_JOB_EXECUTOR;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_IDENTITY_SERVICE_CFG_FILE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.ENGINE_IDENTITY_SERVICE_CLASS_NAME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.Activiti.PETALS_SENDER_COMP_NAME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DATABASE_SCHEMA_UPDATE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DATABASE_TYPE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_DATABASE_SCHEMA_UPDATE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_JDBC_MAX_ACTIVE_CONNECTIONS;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_JDBC_MAX_CHECKOUT_TIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_JDBC_MAX_IDLE_CONNECTIONS;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_JDBC_MAX_WAIT_TIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_JDBC_URL_DATABASE_FILENAME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_DRIVER;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_MAX_ACTIVE_CONNECTIONS;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_MAX_CHECKOUT_TIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_MAX_IDLE_CONNECTIONS;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_MAX_WAIT_TIME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_PASSWORD;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_URL;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.JDBC_USERNAME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_ACTIVATEPROCESSINSTANCES;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_GETPROCESSINSTANCES;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_GETTASKS;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_SUSPENDPROCESSINSTANCES;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE_NAME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_TASK_PORT_TYPE_NAME;

import java.io.File;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.xml.namespace.QName;

import org.activiti.engine.ActivitiException;
import org.activiti.engine.ProcessEngine;
import org.activiti.engine.ProcessEngineConfiguration;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.impl.asyncexecutor.AsyncExecutor;
import org.activiti.engine.impl.asyncexecutor.DefaultAsyncJobExecutor;
import org.activiti.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.activiti.engine.parse.BpmnParseHandler;
import org.apache.cxf.Bus;
import org.apache.cxf.BusFactory;
import org.apache.cxf.transport.ConduitInitiatorManager;
import org.apache.ibatis.datasource.pooled.PooledDataSource;
import org.ow2.easywsdl.wsdl.api.Endpoint;
import org.ow2.easywsdl.wsdl.api.WSDLException;
import org.ow2.petals.activitibpmn.event.AbstractEventListener;
import org.ow2.petals.activitibpmn.event.ProcessInstanceCanceledEventListener;
import org.ow2.petals.activitibpmn.event.ProcessInstanceCompletedEventListener;
import org.ow2.petals.activitibpmn.event.ProcessInstanceStartedEventListener;
import org.ow2.petals.activitibpmn.event.ServiceTaskStartedEventListener;
import org.ow2.petals.activitibpmn.event.UserTaskCompletedEventListener;
import org.ow2.petals.activitibpmn.event.UserTaskStartedEventListener;
import org.ow2.petals.activitibpmn.identity.IdentityService;
import org.ow2.petals.activitibpmn.identity.exception.IdentityServiceInitException;
import org.ow2.petals.activitibpmn.identity.file.FileConfigurator;
import org.ow2.petals.activitibpmn.incoming.ActivitiService;
import org.ow2.petals.activitibpmn.incoming.integration.ActivateProcessInstancesOperation;
import org.ow2.petals.activitibpmn.incoming.integration.GetProcessInstancesOperation;
import org.ow2.petals.activitibpmn.incoming.integration.GetTasksOperation;
import org.ow2.petals.activitibpmn.incoming.integration.SuspendProcessInstancesOperation;
import org.ow2.petals.activitibpmn.incoming.integration.exception.OperationInitializationException;
import org.ow2.petals.activitibpmn.monitoring.Monitoring;
import org.ow2.petals.activitibpmn.outgoing.PetalsSender;
import org.ow2.petals.activitibpmn.outgoing.cxf.transport.PetalsCxfTransportFactory;
import org.ow2.petals.component.framework.listener.AbstractListener;
import org.ow2.petals.component.framework.se.AbstractServiceEngine;
import org.ow2.petals.component.framework.se.ServiceEngineServiceUnitManager;
import org.ow2.petals.component.framework.util.ServiceEndpointOperationKey;
import org.ow2.petals.component.framework.util.WSDLUtilImpl;
import org.ow2.petals.probes.api.exceptions.MultipleProbesFactoriesFoundException;
import org.ow2.petals.probes.api.exceptions.NoProbesFactoryFoundException;

import com.ebmwebsourcing.easycommons.uuid.SimpleUUIDGenerator;

/**
 * The component class of the Activiti BPMN Service Engine.
 * 
 * @author Bertrand Escudie - Linagora
 */
public class ActivitiSE extends AbstractServiceEngine {

    /**
     * The Activiti BPMN Engine.
     */
    private ProcessEngine activitiEngine;

    /**
     * A map used to get the Activiti Operation associated with (end-point Name + Operation)
     */
    private final Map<ServiceEndpointOperationKey, ActivitiService> activitiServices = new ConcurrentHashMap<ServiceEndpointOperationKey, ActivitiService>();

    /**
     * The Activiti Async Executor service
     */
    private AsyncExecutor activitiAsyncExecutor = null;

    /**
     * Activation flag of the Activiti job executor
     */
    private boolean enableActivitiJobExecutor = DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR;

    /**
     * Event listener fired when a process is started
     */
    private AbstractEventListener processInstanceStartedEventListener;

    /**
     * Event listener fired when a process is completed
     */
    private AbstractEventListener processInstanceCompletedEventListener;

    /**
     * Event listener fired when a process is canceled
     */
    private AbstractEventListener processInstanceCanceledEventListener;

    /**
     * Event listener fired when a service task is started
     */
    private AbstractEventListener serviceTaskStartedEventListener;

    /**
     * Event listener fired when a user task is started
     */
    private AbstractEventListener userTaskStartedEventListener;

    /**
     * Event listener fired when a user task is completed
     */
    private AbstractEventListener userTaskCompletedEventListener;

    /**
     * An UUID generator.
     */
    private final SimpleUUIDGenerator simpleUUIDGenerator = new SimpleUUIDGenerator();

    /**
     * @return the Activiti Engine
     */
    public ProcessEngine getProcessEngine() {
        return this.activitiEngine;
    }

    /**
     * @param eptAndOperation
     *            the end-point Name and operation Name
     * @param activitiservice
     *            the Activiti service
     * @return the map with the inserted elements
     */
    public void registerActivitiService(final ServiceEndpointOperationKey eptAndOperation,
            final ActivitiService activitiservice) {
        this.activitiServices.put(eptAndOperation, activitiservice);
    }

    /**
     * @param eptName
     *            the end-point name
     */
    public void removeActivitiService(final String eptName) {

        final Iterator<Entry<ServiceEndpointOperationKey, ActivitiService>> itEptOperationToActivitiOperation = this.activitiServices
                .entrySet().iterator();
        while (itEptOperationToActivitiOperation.hasNext()) {
            final Entry<ServiceEndpointOperationKey, ActivitiService> entry = itEptOperationToActivitiOperation.next();
            if (entry.getKey().getEndpointName().equals(eptName)) {
                itEptOperationToActivitiOperation.remove();
            }
        }
    }

    /**
     * @param logLevel
     */
    public void logEptOperationToActivitiOperation(final Logger logger, final Level logLevel) {
        if (logger.isLoggable(logLevel)) {
            for (final Map.Entry<ServiceEndpointOperationKey, ActivitiService> entry : this.activitiServices
                    .entrySet()) {
                final ServiceEndpointOperationKey key = entry.getKey();
                logger.log(logLevel, "*** Endpoint Operation ");
                logger.log(logLevel, key.toString());
                logger.log(logLevel, "------------------------------------------------------ ");
                entry.getValue().log(logger, logLevel);
                logger.log(logLevel, "******************* ");
            }
        }
    }

    /**
     * @param eptAndOperation
     *            the end-point Name and operation Name
     * @return the Activiti Service associated with this end-point name and operation Name
     */
    public ActivitiService getActivitiServices(final ServiceEndpointOperationKey eptAndOperation) {
        return this.activitiServices.get(eptAndOperation);
    }

    @Override
    public void doInit() throws JBIException {
        this.getLogger().fine("Start ActivitiSE.doInit()");

        try {
            // JDBC Driver
            final String jdbcDriver = ActivitiParameterReader
                    .getJdbcDriver(this.getComponentExtensions().get(JDBC_DRIVER), this.getLogger());

            // JDBC URL
            final String jdbcUrlConfigured = this.getComponentExtensions().get(JDBC_URL);
            final String jdbcUrl;
            if (jdbcUrlConfigured == null || jdbcUrlConfigured.trim().isEmpty()) {
                // JDBC URL not set or empty ---> Default value:
                // $PETALS_HOME/data/repository/components/<se-bpmn>/h2-activiti.db
                this.getLogger().info("No JDBC URL configured for database. Default value used.");
                final File databaseFile = new File(this.getContext().getWorkspaceRoot(),
                        DEFAULT_JDBC_URL_DATABASE_FILENAME);
                try {
                    jdbcUrl = String.format("jdbc:h2:%s", databaseFile.toURI().toURL().toExternalForm());
                } catch (final MalformedURLException e) {
                    // This exception should not occur. It's a bug
                    throw new JBIException("The defaul JDBC URL is invalid !!", e);
                }
            } else {
                jdbcUrl = jdbcUrlConfigured;
            }

            final String jdbcUsername = this.getComponentExtensions().get(JDBC_USERNAME);
            final String jdbcPassword = this.getComponentExtensions().get(JDBC_PASSWORD);

            final String jdbcMaxActiveConnectionsConfigured = this.getComponentExtensions()
                    .get(JDBC_MAX_ACTIVE_CONNECTIONS);
            int jdbcMaxActiveConnections;
            if (jdbcMaxActiveConnectionsConfigured == null || jdbcMaxActiveConnectionsConfigured.trim().isEmpty()) {
                this.getLogger().info("No JDBC Max Active Connections configured for database. Default value used.");
                jdbcMaxActiveConnections = DEFAULT_JDBC_MAX_ACTIVE_CONNECTIONS;
            } else {
                try {
                    jdbcMaxActiveConnections = Integer.parseInt(jdbcMaxActiveConnectionsConfigured);
                } catch (final NumberFormatException e) {
                    this.getLogger().warning(
                            "Invalid value for the number of JDBC Max Active Connections. Default value used.");
                    jdbcMaxActiveConnections = DEFAULT_JDBC_MAX_ACTIVE_CONNECTIONS;
                }
            }

            final String jdbcMaxIdleConnectionsConfigured = this.getComponentExtensions()
                    .get(JDBC_MAX_IDLE_CONNECTIONS);
            int jdbcMaxIdleConnections;
            if (jdbcMaxIdleConnectionsConfigured == null || jdbcMaxIdleConnectionsConfigured.trim().isEmpty()) {
                this.getLogger().info("No JDBC Max Idle Connections configured for database. Default value used.");
                jdbcMaxIdleConnections = DEFAULT_JDBC_MAX_IDLE_CONNECTIONS;
            } else {
                try {
                    jdbcMaxIdleConnections = Integer.parseInt(jdbcMaxIdleConnectionsConfigured);
                } catch (final NumberFormatException e) {
                    this.getLogger()
                            .warning("Invalid value for the number of JDBC Max Idle Connections. Default value used.");
                    jdbcMaxIdleConnections = DEFAULT_JDBC_MAX_IDLE_CONNECTIONS;
                }
            }

            final String jdbcMaxCheckoutTimeConfigured = this.getComponentExtensions().get(JDBC_MAX_CHECKOUT_TIME);
            int jdbcMaxCheckoutTime;
            if (jdbcMaxCheckoutTimeConfigured == null || jdbcMaxCheckoutTimeConfigured.trim().isEmpty()) {
                this.getLogger().info("No JDBC Max Checkout Time configured for database. Default value used.");
                jdbcMaxCheckoutTime = DEFAULT_JDBC_MAX_CHECKOUT_TIME;
            } else {
                try {
                    jdbcMaxCheckoutTime = Integer.parseInt(jdbcMaxCheckoutTimeConfigured);
                } catch (final NumberFormatException e) {
                    this.getLogger()
                            .warning("Invalid value for the number of JDBC Max Checkout Time. Default value used.");
                    jdbcMaxCheckoutTime = DEFAULT_JDBC_MAX_CHECKOUT_TIME;
                }
            }

            final String jdbcMaxWaitTimeConfigured = this.getComponentExtensions().get(JDBC_MAX_WAIT_TIME);
            int jdbcMaxWaitTime;
            if (jdbcMaxWaitTimeConfigured == null || jdbcMaxWaitTimeConfigured.trim().isEmpty()) {
                this.getLogger().info("No JDBC Max Wait Time configured for database. Default value used.");
                jdbcMaxWaitTime = DEFAULT_JDBC_MAX_WAIT_TIME;
            } else {
                try {
                    jdbcMaxWaitTime = Integer.parseInt(jdbcMaxWaitTimeConfigured);
                } catch (final NumberFormatException e) {
                    this.getLogger().warning("Invalid value for the number of JDBC Max Wait Time. Default value used.");
                    jdbcMaxWaitTime = DEFAULT_JDBC_MAX_WAIT_TIME;
                }
            }

            /* DATABASE_TYPE Possible values: {h2, mysql, oracle, postgres, mssql, db2}. */
            final String databaseType = this.getComponentExtensions().get(DATABASE_TYPE);

            /* DATABASE_SCHEMA_UPDATE Possible values: {false, true, create-drop } */
            /*
             * TODO Test the Database Schema Version What about databaseSchemaUpdate values "true" and "create-drop"
             */
            final String databaseSchemaUpdateConfigured = this.getComponentExtensions().get(DATABASE_SCHEMA_UPDATE);
            final String databaseSchemaUpdate;
            if (databaseSchemaUpdateConfigured == null || databaseSchemaUpdateConfigured.trim().isEmpty()) {
                this.getLogger().info("No schema update processing configured for database. Default value used.");
                databaseSchemaUpdate = DEFAULT_DATABASE_SCHEMA_UPDATE;
            } else if (databaseSchemaUpdateConfigured.trim().equals("false")
                    || databaseSchemaUpdateConfigured.trim().equals("true")
                    || databaseSchemaUpdateConfigured.trim().equals("create-drop")) {
                databaseSchemaUpdate = databaseSchemaUpdateConfigured.trim();
            } else {
                this.getLogger().info("Invalid value '" + databaseSchemaUpdateConfigured
                        + "' configured for the schema update processing. Default value used.");
                databaseSchemaUpdate = DEFAULT_DATABASE_SCHEMA_UPDATE;
            }

            /* TODO Test Activiti database connection configuration */
            /* TODO Set the non set value with default value */

            this.getLogger().config("DB configuration:");
            this.getLogger().config("   - " + JDBC_DRIVER + " = " + jdbcDriver);
            this.getLogger().config("   - " + JDBC_URL + " = " + jdbcUrl);
            this.getLogger().config("   - " + JDBC_USERNAME + " = " + jdbcUsername);
            this.getLogger().config("   - " + JDBC_PASSWORD + " = " + jdbcPassword);
            this.getLogger().config("   - " + JDBC_MAX_ACTIVE_CONNECTIONS + " = " + jdbcMaxActiveConnections);
            this.getLogger().config("   - " + JDBC_MAX_IDLE_CONNECTIONS + " = " + jdbcMaxIdleConnections);
            this.getLogger().config("   - " + JDBC_MAX_CHECKOUT_TIME + " = " + jdbcMaxCheckoutTime);
            this.getLogger().config("   - " + JDBC_MAX_WAIT_TIME + " = " + jdbcMaxWaitTime);
            this.getLogger().config("   - " + DATABASE_TYPE + " = " + databaseType);
            this.getLogger().config("   - " + DATABASE_SCHEMA_UPDATE + " = " + databaseSchemaUpdate);

            // Caution:
            // - only the value "false", ignoring case and spaces will disable the job executor,
            // - only the value "true", ignoring case and spaces will enable the job executor,
            // - otherwise, the default value is used.
            final String enableActivitiJobExecutorConfigured = this.getComponentExtensions()
                    .get(ENGINE_ENABLE_JOB_EXECUTOR);
            if (enableActivitiJobExecutorConfigured == null || enableActivitiJobExecutorConfigured.trim().isEmpty()) {
                this.getLogger()
                        .info("The activation of the Activiti job executor is not configured. Default value used.");
                this.enableActivitiJobExecutor = DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR;
            } else {
                this.enableActivitiJobExecutor = enableActivitiJobExecutorConfigured.trim().equalsIgnoreCase("false")
                        ? false
                        : (enableActivitiJobExecutorConfigured.trim().equalsIgnoreCase("true") ? true
                                : DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR);
            }

            // Caution:
            // - only the value "false", ignoring case and spaces will disable the BPMN validation,
            // - only the value "true", ignoring case and spaces will enable the BPMN validation,
            // - otherwise, the default value is used.
            final String enableActivitiBpmnValidationConfigured = this.getComponentExtensions()
                    .get(ENGINE_ENABLE_BPMN_VALIDATION);
            final boolean enableActivitiBpmnValidation;
            if (enableActivitiBpmnValidationConfigured == null
                    || enableActivitiBpmnValidationConfigured.trim().isEmpty()) {
                this.getLogger().info(
                        "The activation of the BPMN validation during process deployments is not configured. Default value used.");
                enableActivitiBpmnValidation = DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION;
            } else {
                enableActivitiBpmnValidation = enableActivitiBpmnValidationConfigured.trim().equalsIgnoreCase("false")
                        ? false
                        : (enableActivitiBpmnValidationConfigured.trim().equalsIgnoreCase("true") ? true
                                : DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION);
            }

            ((ActivitiSuManager) getServiceUnitManager()).setEnableActivitiBpmnValidation(enableActivitiBpmnValidation);

            final Class<?> identityServiceClass = ActivitiParameterReader.getEngineIdentityServiceClassName(
                    this.getComponentExtensions().get(ENGINE_IDENTITY_SERVICE_CLASS_NAME), this.getLogger());

            final File identityServiceCfgFile = ActivitiParameterReader.getEngineIdentityServiceConfigurationFile(
                    this.getComponentExtensions().get(ENGINE_IDENTITY_SERVICE_CFG_FILE), this.getLogger());

            this.getLogger().config("Activiti engine configuration:");
            this.getLogger().config("   - " + ENGINE_ENABLE_JOB_EXECUTOR + " = " + this.enableActivitiJobExecutor);
            this.getLogger().config("   - " + ENGINE_ENABLE_BPMN_VALIDATION + " = " + enableActivitiBpmnValidation);
            this.getLogger()
                    .config("   - " + ENGINE_IDENTITY_SERVICE_CLASS_NAME + " = " + identityServiceClass.getName());
            this.getLogger().config("   - " + ENGINE_IDENTITY_SERVICE_CFG_FILE + " = "
                    + (identityServiceCfgFile == null ? "<null>" : identityServiceCfgFile.getAbsolutePath()));

            /* Create an Activiti ProcessEngine with database configuration */
            final ProcessEngineConfiguration pec = ProcessEngineConfiguration
                    .createStandaloneProcessEngineConfiguration();
            pec.setJdbcDriver(jdbcDriver);
            pec.setJdbcUrl(jdbcUrl);
            pec.setJdbcUsername(jdbcUsername).setJdbcPassword(jdbcPassword);
            pec.setJdbcMaxActiveConnections(jdbcMaxActiveConnections);
            pec.setJdbcMaxIdleConnections(jdbcMaxIdleConnections);
            pec.setJdbcMaxCheckoutTime(jdbcMaxCheckoutTime);
            pec.setJdbcMaxWaitTime(jdbcMaxWaitTime);
            if (databaseType != null && !databaseType.trim().isEmpty()) {
                pec.setDatabaseType(databaseType);
            }
            pec.setDatabaseSchemaUpdate(databaseSchemaUpdate);

            // We register the Petals transport into Apache CXF
            this.registerCxfPetalsTransport();

            // As recommended by Activiti team, we prefer the Async Job Executor
            pec.setJobExecutorActivate(false);
            // The Async job is enabled ...
            pec.setAsyncExecutorEnabled(this.enableActivitiJobExecutor);
            // ... but must be started when starting the SE
            pec.setAsyncExecutorActivate(false);

            // Override the default configuration of the identity service.
            this.registerIdentityService(pec, identityServiceClass, identityServiceCfgFile);

            // Add post BPMN parse handlers
            this.addPostBpmnParseHandlers(pec);

            this.activitiEngine = pec.buildProcessEngine();
            if (this.enableActivitiJobExecutor) {
                this.activitiAsyncExecutor = pec.getAsyncExecutor();
                this.configureAsyncExecutor();
            } else {
                this.activitiAsyncExecutor = null;
            }

            // Caution: Configuration beans are initialized when building the process engine
            if (pec instanceof ProcessEngineConfigurationImpl) {
                // We add to the BPMN engine the bean in charge of sending Petals message exchange
                final AbstractListener petalsSender = new PetalsSender(this);
                ((ProcessEngineConfigurationImpl) pec).getBeans().put(PETALS_SENDER_COMP_NAME, petalsSender);
            } else {
                this.getLogger().warning(
                        "The implementation of the process engine configuration is not the expected one ! No Petals services can be invoked !");
            }

            // Configure a part of the monitoring MBean. Another part is configured on component startup.
            ((Monitoring) this.getMonitoringBean()).setActivitiEngine(this.activitiEngine);

            this.registersIntegrationOperations();

        } catch (final ActivitiException e) {
            throw new JBIException("An error occurred while creating the Activiti BPMN Engine.", e);
        } finally {
            this.getLogger().fine("End ActivitiSE.doInit()");
        }
    }

    /**
     * Registers integration operations
     */
    private void registersIntegrationOperations() {

        // Register integration operation
        final List<Endpoint> integrationEndpoints = WSDLUtilImpl.getEndpointList(this.getNativeWsdl().getDescription());
        if (integrationEndpoints.size() > 0) {
            try {
                for (final Endpoint endpoint : integrationEndpoints) {
                    final String integrationEndpointName = endpoint.getName();
                    final QName integrationServiceName = endpoint.getService().getQName();
                    final QName integrationInterfaceName = endpoint.getService().getInterface().getQName();
                    try {
                        if (ITG_PROCESSINSTANCES_PORT_TYPE_NAME.equals(integrationInterfaceName.getLocalPart())) {
                            this.activitiServices.put(
                                    new ServiceEndpointOperationKey(integrationServiceName, integrationEndpointName,
                                            ITG_OP_GETPROCESSINSTANCES),
                                    new GetProcessInstancesOperation(this.activitiEngine.getRuntimeService(),
                                            this.activitiEngine.getHistoryService(),
                                            this.activitiEngine.getRepositoryService(), this.getLogger()));
                            this.activitiServices.put(
                                    new ServiceEndpointOperationKey(integrationServiceName, integrationEndpointName,
                                            ITG_OP_SUSPENDPROCESSINSTANCES),
                                    new SuspendProcessInstancesOperation(this.activitiEngine.getRuntimeService(),
                                            this.getLogger()));
                            this.activitiServices.put(
                                    new ServiceEndpointOperationKey(integrationServiceName, integrationEndpointName,
                                            ITG_OP_ACTIVATEPROCESSINSTANCES),
                                    new ActivateProcessInstancesOperation(this.activitiEngine.getRuntimeService(),
                                            this.getLogger()));
                        } else if (ITG_TASK_PORT_TYPE_NAME.equals(integrationInterfaceName.getLocalPart())) {
                            this.activitiServices.put(
                                    new ServiceEndpointOperationKey(integrationServiceName, integrationEndpointName,
                                            ITG_OP_GETTASKS),
                                    new GetTasksOperation(this.activitiEngine.getTaskService(),
                                            this.activitiEngine.getRepositoryService(), this.getLogger()));
                        } else {
                            this.getLogger().log(Level.WARNING,
                                    "Unexpected/Uknown integration operations: " + integrationInterfaceName);
                        }
                    } catch (final OperationInitializationException e) {
                        this.getLogger().log(Level.WARNING,
                                "Error registering the integration operation '" + integrationInterfaceName + "'.", e);
                    }
                }
            } catch (final WSDLException e) {
                this.getLogger().log(Level.WARNING, "Integration operations are not completly initialized", e);
            }
        } else {
            this.getLogger().warning("No endpoint exists to execute integration operations");
        }
    }

    /**
     * Initialize the identity service
     * 
     * @param pec
     *            The Activiti process engine configuration. Not <code>null</code>.
     * @param identityServiceClass
     *            The identity service class to use. Not <code>null</code>. Must implement {@link IdentityService}.
     * @param identityServiceCfgFile
     *            The identity service configuration file. If <code>null</code>, the default configuration of the
     *            identity service will be used.
     */
    private final void registerIdentityService(final ProcessEngineConfiguration pec,
            final Class<?> identityServiceClass, final File identityServiceCfgFile) throws JBIException {

        assert pec != null : "pec can not be null";
        assert identityServiceClass != null : "identityServiceClass can not be null";
        assert IdentityService.class.isAssignableFrom(
                identityServiceClass) : "The identity service class does not implement IdentityService";

        Object identityServiceObj;
        try {
            identityServiceObj = identityServiceClass.newInstance();
            assert identityServiceObj instanceof IdentityService;
            final IdentityService identityService = (IdentityService) identityServiceObj;

            identityService.init(identityServiceCfgFile);

            if (pec instanceof ProcessEngineConfigurationImpl) {
                ((ProcessEngineConfigurationImpl) pec).addConfigurator(new FileConfigurator(identityService));
            } else {
                this.getLogger().warning(
                        "The implementation of the process engine configuration is not the expected one ! Identity service not overriden !");
            }
        } catch (final InstantiationException | IllegalAccessException | IdentityServiceInitException e) {
            throw new JBIException("An error occurred while instantiating the identity service.", e);
        }
    }

    /**
     * Add BPMN parse handlers executed after the default ones
     * 
     * @param pec
     *            The Activiti process engine configuration. Not <code>null</code>.
     */
    private final void addPostBpmnParseHandlers(final ProcessEngineConfiguration pec) throws JBIException {

        assert pec != null : "pec can not be null";

        if (pec instanceof ProcessEngineConfigurationImpl) {
            final List<BpmnParseHandler> postBpmnParseHandlers = new ArrayList<BpmnParseHandler>();
            postBpmnParseHandlers.add(new ServiceTaskForceAsyncParseHandler());
            ((ProcessEngineConfigurationImpl) pec).setPostBpmnParseHandlers(postBpmnParseHandlers);
        } else {
            this.getLogger().warning(
                    "The implementation of the process engine configuration is not the expected one ! Identity service not overriden !");
        }
    }

    @Override
    public void doStart() throws JBIException {
        this.getLogger().fine("Start ActivitiSE.doStart()");

        // Create & Register event listeners
        final RuntimeService runtimeService = this.activitiEngine.getRuntimeService();
        this.processInstanceStartedEventListener = new ProcessInstanceStartedEventListener(this.getLogger());
        runtimeService.addEventListener(this.processInstanceStartedEventListener,
                this.processInstanceStartedEventListener.getListenEventType());

        this.processInstanceCompletedEventListener = new ProcessInstanceCompletedEventListener(this.getLogger());
        runtimeService.addEventListener(this.processInstanceCompletedEventListener,
                this.processInstanceCompletedEventListener.getListenEventType());

        this.processInstanceCanceledEventListener = new ProcessInstanceCanceledEventListener(this.getLogger());
        runtimeService.addEventListener(this.processInstanceCanceledEventListener,
                this.processInstanceCanceledEventListener.getListenEventType());

        this.serviceTaskStartedEventListener = new ServiceTaskStartedEventListener(this.getLogger());
        runtimeService.addEventListener(this.serviceTaskStartedEventListener,
                this.serviceTaskStartedEventListener.getListenEventType());

        this.userTaskStartedEventListener = new UserTaskStartedEventListener(this.simpleUUIDGenerator,
                this.getLogger());
        runtimeService.addEventListener(this.userTaskStartedEventListener,
                this.userTaskStartedEventListener.getListenEventType());

        this.userTaskCompletedEventListener = new UserTaskCompletedEventListener(this.getLogger());
        runtimeService.addEventListener(this.userTaskCompletedEventListener,
                this.userTaskCompletedEventListener.getListenEventType());

        try {
            // Startup Activiti engine against running states of the SE:
            // - Activiti Engine must be started when the SE is in state 'STOPPED' to be able to deploy process
            // definitions
            // - In state 'STOPPED', the SE will not process incoming requests, so no process instance creation and no
            // user task completion will occur
            // - To avoid the executions of activities trigerred by timer or other events, the Activiti job executor
            // must be stopped when the SE is in state 'STOPPED'
            if (this.enableActivitiJobExecutor) {
                if (this.activitiAsyncExecutor != null) {
                    if (this.activitiAsyncExecutor.isActive()) {
                        this.getLogger().warning("Activiti Job Executor already started !!");
                    } else {
                        this.activitiAsyncExecutor.start();
                        this.configureMonitoringMBeanWithAsyncExecutorThreadPool();
                        this.configureMonitoringMBeanWithDatabaseConnectionPool();
                    }
                } else {
                    this.getLogger().warning("No Activiti Job Executor exists !!");
                }
            } else {
                this.getLogger().info("Activiti Job Executor not started because it is not activated.");
            }

            // TODO: Add JMX operation to start/stop the Activiti job executor when the component is started
            // TODO: Add JMX operation to disable/enable the Activiti job executor when the component is running

        } catch (final ActivitiException e) {
            throw new JBIException("An error occurred while starting the Activiti BPMN Engine.", e);
        } finally {
            this.getLogger().fine("End ActivitiSE.doStart()");
        }
    }

    /**
     * COnfigure the asynchronous executor
     */
    private void configureAsyncExecutor() {
        if (this.activitiAsyncExecutor != null) {
            if (this.activitiAsyncExecutor instanceof DefaultAsyncJobExecutor) {

                final int corePoolSize = this.getAsyncJobExecutorCorePoolSize();
                final int maxPoolSize = this.getAsyncJobExecutorMaxPoolSize();
                final long keepAliveTime = this.getAsyncJobExecutorKeepAliveTime();
                final int queueSize = this.getAsyncJobExecutorQueueSize();
                final int maxTimerJobsPerAcquisition = this.getAsyncJobExecutorMaxTimerJobsPerAcquisition();
                final int maxAsyncJobsDuePerAcquisition = this.getAsyncJobExecutorMaxAsyncJobsDuePerAcquisition();
                final int asyncJobAcquireWaitTime = this.getAsyncJobExecutorAsyncJobAcquireWaitTime();
                final int timerJobAcquireWaitTime = this.getAsyncJobExecutorTimerJobAcquireWaitTime();
                final int timerLockTime = this.getAsyncJobExecutorTimerLockTime();
                final int asyncJobLockTime = this.getAsyncJobExecutorAsyncJobLockTime();

                final Logger logger = this.getLogger();
                logger.config("Asynchronous job executor configuration:");
                logger.config("   - " + ActivitiSEConstants.ENGINE_JOB_EXECUTOR_COREPOOLSIZE + " = " + corePoolSize);
                logger.config("   - " + ActivitiSEConstants.ENGINE_JOB_EXECUTOR_MAXPOOLSIZE + " = " + maxPoolSize);
                logger.config("   - " + ActivitiSEConstants.ENGINE_JOB_EXECUTOR_KEEPALIVETIME + " = " + keepAliveTime);
                logger.config("   - " + ActivitiSEConstants.ENGINE_JOB_EXECUTOR_QUEUESIZE + " = " + queueSize);
                logger.config("   - " + ActivitiSEConstants.ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION + " = "
                        + maxTimerJobsPerAcquisition);
                logger.config("   - " + ActivitiSEConstants.ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION + " = "
                        + maxAsyncJobsDuePerAcquisition);
                logger.config("   - " + ActivitiSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME + " = "
                        + asyncJobAcquireWaitTime);
                logger.config("   - " + ActivitiSEConstants.ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME + " = "
                        + timerJobAcquireWaitTime);
                logger.config("   - " + ActivitiSEConstants.ENGINE_JOB_EXECUTOR_TIMERLOCKTIME + " = " + timerLockTime);
                logger.config(
                        "   - " + ActivitiSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME + " = " + asyncJobLockTime);

                final DefaultAsyncJobExecutor defaultAsyncJobExecutor = (DefaultAsyncJobExecutor) this.activitiAsyncExecutor;
                defaultAsyncJobExecutor.setCorePoolSize(corePoolSize);
                defaultAsyncJobExecutor.setMaxPoolSize(maxPoolSize);
                defaultAsyncJobExecutor.setKeepAliveTime(keepAliveTime);
                defaultAsyncJobExecutor.setQueueSize(queueSize);
                defaultAsyncJobExecutor.setMaxTimerJobsPerAcquisition(maxTimerJobsPerAcquisition);
                defaultAsyncJobExecutor.setMaxAsyncJobsDuePerAcquisition(maxAsyncJobsDuePerAcquisition);
                defaultAsyncJobExecutor.setDefaultAsyncJobAcquireWaitTimeInMillis(asyncJobAcquireWaitTime);
                defaultAsyncJobExecutor.setDefaultTimerJobAcquireWaitTimeInMillis(timerJobAcquireWaitTime);
                defaultAsyncJobExecutor.setTimerLockTimeInMillis(timerLockTime);
                defaultAsyncJobExecutor.setAsyncJobLockTimeInMillis(asyncJobLockTime);
            } else {
                this.getLogger().warning(
                        "The implementation of the asynchronous job executor is not the expected one, so no configuration is needed !");
            }
        } else {
            this.getLogger().warning("No asynchronous job executor available, so no configuration is needed !");
        }
    }

    /**
     * Configure the monitoring MBean with the asynchronous executor thread pool
     */
    private void configureMonitoringMBeanWithAsyncExecutorThreadPool() {

        if (this.activitiAsyncExecutor != null) {
            if (this.activitiAsyncExecutor instanceof DefaultAsyncJobExecutor) {
                final ExecutorService executorService = ((DefaultAsyncJobExecutor) this.activitiAsyncExecutor)
                        .getExecutorService();
                if (executorService == null) {
                    this.getLogger().warning(
                            "No executor service available for the asynchronous job executor, so no monitoring available on asynchronous executor !");
                } else if (executorService instanceof ThreadPoolExecutor) {
                    ((Monitoring) this.getMonitoringBean())
                            .setAsyncExecutorTreadPool((ThreadPoolExecutor) executorService);
                } else {
                    this.getLogger().warning(
                            "The implementation of the executor service of the asynchronous job executor is not the expected one ! Failures can occurs on monitoring parts !");
                }
            } else {
                this.getLogger().warning(
                        "The implementation of the asynchronous job executor is not the expected one, so no monitoring available on asynchronous executor !");
            }
        } else {
            this.getLogger().warning(
                    "No asynchronous job executor available, so no monitoring available on asynchronous executor !");
        }
    }

    /**
     * Configure the monitoring MBean with the database connection pool
     */
    private void configureMonitoringMBeanWithDatabaseConnectionPool() {

        if (this.activitiEngine.getProcessEngineConfiguration() != null) {
            if (this.activitiEngine.getProcessEngineConfiguration() instanceof ProcessEngineConfigurationImpl) {
                final ProcessEngineConfigurationImpl engineConfiguration = (ProcessEngineConfigurationImpl) this.activitiEngine
                        .getProcessEngineConfiguration();
                if (engineConfiguration.getDataSource() == null) {
                    this.getLogger().warning(
                            "No datasource available for the Activiti engine, so no monitoring available on database connection pool !");
                } else if (engineConfiguration.getDataSource() instanceof PooledDataSource) {
                    ((Monitoring) this.getMonitoringBean())
                            .setDataSourcePool((PooledDataSource) engineConfiguration.getDataSource());
                } else {
                    this.getLogger().warning(
                            "The implementation of the Activiti engine datasource is not the expected one, so no monitoring available on database connection pool !");
                }
            } else {
                this.getLogger().warning(
                        "The implementation of the engine configuration is not the expected one, so no monitoring available on database connection pool !");
            }
        } else {
            this.getLogger().warning(
                    "No process engine configuration, so no monitoring available on database connection pool !");
        }
    }

    @Override
    public void doStop() throws JBIException {
        this.getLogger().fine("Start ActivitiSE.doStop()");

        try {
            // Stop the Activiti Job Executor */
            if (this.enableActivitiJobExecutor) {
                if (this.activitiAsyncExecutor != null) {
                    if (this.activitiAsyncExecutor.isActive()) {
                        this.activitiAsyncExecutor.shutdown();
                    } else {
                        this.getLogger().warning("Activiti Job Executor not started !!");
                    }
                } else {
                    this.getLogger().warning("No Activiti Job Executor exists !!");
                }
            } else {
                this.getLogger().info("Activiti Job Executor not stopped because it is not activated.");
            }

        } catch (final ActivitiException e) {
            throw new JBIException("An error occurred while stopping the Activiti BPMN Engine.", e);
        } finally {
            this.getLogger().fine("End ActivitiSE.doStop()");
        }

        // Deregister event listeners
        final RuntimeService runtimeService = this.activitiEngine.getRuntimeService();
        runtimeService.removeEventListener(this.processInstanceStartedEventListener);
        runtimeService.removeEventListener(this.processInstanceCompletedEventListener);
        runtimeService.removeEventListener(this.processInstanceCanceledEventListener);
        runtimeService.removeEventListener(this.serviceTaskStartedEventListener);
        runtimeService.removeEventListener(this.userTaskStartedEventListener);
        runtimeService.removeEventListener(this.userTaskCompletedEventListener);

    }

    @Override
    public void doShutdown() throws JBIException {
        this.getLogger().fine("Start ActivitiSE.doShutdown()");

        try {
            if (this.activitiEngine != null) {
                this.activitiEngine.close();
            }

        } catch (final ActivitiException e) {
            throw new JBIException("An error occurred while shutdowning the Activiti BPMN Engine.", e);
        } finally {
            this.getLogger().fine("End ActivitiSE.doShutdown()");
        }
    }

    @Override
    protected ServiceEngineServiceUnitManager createServiceUnitManager() {
        return new ActivitiSuManager(this, this.simpleUUIDGenerator);
    }

    private void registerCxfPetalsTransport() {
        final Bus bus = BusFactory.getThreadDefaultBus();
        final PetalsCxfTransportFactory cxfPetalsTransport = new PetalsCxfTransportFactory();
        final ConduitInitiatorManager extension = bus.getExtension(ConduitInitiatorManager.class);
        extension.registerConduitInitiator(PetalsCxfTransportFactory.TRANSPORT_ID, cxfPetalsTransport);
        // TODO: Set a timeout at CXF client level (it should be the same than the tiemout at NMR level)
        // TODO: Add unit tests about timeout
    }

    @Override
    protected org.ow2.petals.component.framework.monitoring.Monitoring createMonitoringMBean()
            throws MultipleProbesFactoriesFoundException, NoProbesFactoryFoundException {

        return new Monitoring(this.getProbesTimer(), this.getResponseTimeProbeSamplePeriod());
    }

    /**
     * @return The core pool size of the asynchronous job executor of the Activiti engine.
     */
    private int getAsyncJobExecutorCorePoolSize() {
        return this.getParameterAsPositiveInteger(ActivitiSEConstants.ENGINE_JOB_EXECUTOR_COREPOOLSIZE,
                ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_COREPOOLSIZE);
    }

    /**
     * @return The max pool size of the asynchronous job executor of the Activiti engine.
     */
    private int getAsyncJobExecutorMaxPoolSize() {
        return this.getParameterAsPositiveInteger(ActivitiSEConstants.ENGINE_JOB_EXECUTOR_MAXPOOLSIZE,
                ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE);
    }

    /**
     * Get the keep-alive time of the asynchronous job executor of the Activiti engine.
     * 
     * @param logger
     * @param configurationExtensions
     *            the component extensions
     * @return the keep alive time of the asynchronous job executor
     */
    private long getAsyncJobExecutorKeepAliveTime() {
        return this.getParameterAsPositiveLong(ActivitiSEConstants.ENGINE_JOB_EXECUTOR_KEEPALIVETIME,
                ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_KEEPALIVETIME);
    }

    /**
     * Get the queue size of the asynchronous job executor of the Activiti engine.
     * 
     * @param logger
     * @param configurationExtensions
     *            the component extensions
     * @return the queue size of the asynchronous job executor
     */
    private int getAsyncJobExecutorQueueSize() {
        return this.getParameterAsPositiveInteger(ActivitiSEConstants.ENGINE_JOB_EXECUTOR_QUEUESIZE,
                ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_QUEUESIZE);
    }

    /**
     * Get the max number of jobs fetched by query of the asynchronous job executor of the Activiti engine.
     * 
     * @param logger
     * @param configurationExtensions
     *            the component extensions
     * @return the max number of jobs fetched by query of the asynchronous job executor
     */
    private int getAsyncJobExecutorMaxTimerJobsPerAcquisition() {
        return this.getParameterAsPositiveInteger(ActivitiSEConstants.ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION,
                ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION);
    }

    /**
     * Get the number of asynchronous jobs due that are fetched by the asynchronous job executor of the Activiti engine.
     * 
     * @param logger
     * @param configurationExtensions
     *            the component extensions
     * @return the number of asynchronous jobs due that are fetched by the asynchronous job executor
     */
    private int getAsyncJobExecutorMaxAsyncJobsDuePerAcquisition() {
        return this.getParameterAsPositiveInteger(ActivitiSEConstants.ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION,
                ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION);
    }

    private int getAsyncJobExecutorAsyncJobAcquireWaitTime() {
        return this.getParameterAsPositiveInteger(ActivitiSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME,
                ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME);
    }

    private int getAsyncJobExecutorTimerJobAcquireWaitTime() {
        return this.getParameterAsPositiveInteger(ActivitiSEConstants.ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME,
                ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME);
    }

    /**
     * Get the timer job lock time of the asynchronous job executor of the Activiti engine.
     * 
     * @param logger
     * @param configurationExtensions
     *            the component extensions
     * @return the timer job lock time of the asynchronous job executor
     */
    private int getAsyncJobExecutorTimerLockTime() {
        return this.getParameterAsPositiveInteger(ActivitiSEConstants.ENGINE_JOB_EXECUTOR_TIMERLOCKTIME,
                ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_TIMERLOCKTIME);
    }

    private int getAsyncJobExecutorAsyncJobLockTime() {
        return this.getParameterAsPositiveInteger(ActivitiSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME,
                ActivitiSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME);
    }
}
