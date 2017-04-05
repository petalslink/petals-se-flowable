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

import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_ENABLE_BPMN_VALIDATION;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_IDENTITY_SERVICE_CFG_FILE;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_IDENTITY_SERVICE_CLASS_NAME;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DATABASE_SCHEMA_UPDATE;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DATABASE_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_DATABASE_SCHEMA_UPDATE;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_JDBC_MAX_ACTIVE_CONNECTIONS;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_JDBC_MAX_CHECKOUT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_JDBC_MAX_IDLE_CONNECTIONS;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_JDBC_MAX_WAIT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_JDBC_URL_DATABASE_FILENAME;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_DRIVER;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_MAX_ACTIVE_CONNECTIONS;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_MAX_CHECKOUT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_MAX_IDLE_CONNECTIONS;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_MAX_WAIT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_PASSWORD;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_URL;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.JDBC_USERNAME;
import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.PETALS_SENDER_COMP_NAME;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_ACTIVATEPROCESSINSTANCES;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETPROCESSINSTANCES;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETTASKS;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_SUSPENDPROCESSINSTANCES;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE_NAME;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_PORT_TYPE_NAME;

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
import org.ow2.petals.component.framework.listener.AbstractListener;
import org.ow2.petals.component.framework.se.AbstractServiceEngine;
import org.ow2.petals.component.framework.se.ServiceEngineServiceUnitManager;
import org.ow2.petals.component.framework.util.ServiceEndpointOperationKey;
import org.ow2.petals.component.framework.util.WSDLUtilImpl;
import org.ow2.petals.flowable.event.AbstractEventListener;
import org.ow2.petals.flowable.event.ProcessInstanceCanceledEventListener;
import org.ow2.petals.flowable.event.ProcessInstanceCompletedEventListener;
import org.ow2.petals.flowable.event.ProcessInstanceStartedEventListener;
import org.ow2.petals.flowable.event.ServiceTaskStartedEventListener;
import org.ow2.petals.flowable.event.UserTaskCompletedEventListener;
import org.ow2.petals.flowable.event.UserTaskStartedEventListener;
import org.ow2.petals.flowable.identity.IdentityService;
import org.ow2.petals.flowable.identity.exception.IdentityServiceInitException;
import org.ow2.petals.flowable.identity.file.FileConfigurator;
import org.ow2.petals.flowable.incoming.FlowableService;
import org.ow2.petals.flowable.incoming.integration.ActivateProcessInstancesOperation;
import org.ow2.petals.flowable.incoming.integration.GetProcessInstancesOperation;
import org.ow2.petals.flowable.incoming.integration.GetTasksOperation;
import org.ow2.petals.flowable.incoming.integration.SuspendProcessInstancesOperation;
import org.ow2.petals.flowable.incoming.integration.exception.OperationInitializationException;
import org.ow2.petals.flowable.monitoring.Monitoring;
import org.ow2.petals.flowable.outgoing.PetalsSender;
import org.ow2.petals.flowable.outgoing.cxf.transport.PetalsCxfTransportFactory;
import org.ow2.petals.probes.api.exceptions.MultipleProbesFactoriesFoundException;
import org.ow2.petals.probes.api.exceptions.NoProbesFactoryFoundException;

import com.ebmwebsourcing.easycommons.uuid.SimpleUUIDGenerator;

/**
 * The component class of the Flowable BPMN Service Engine.
 * 
 * @author Bertrand Escudie - Linagora
 */
public class FlowableSE extends AbstractServiceEngine {

    /**
     * The Flowable BPMN Engine.
     */
    private ProcessEngine flowableEngine;

    /**
     * A map used to get the Flowable Operation associated with (end-point Name + Operation)
     */
    private final Map<ServiceEndpointOperationKey, FlowableService> flowableServices = new ConcurrentHashMap<>();

    /**
     * The Flowable Async Executor service
     */
    private AsyncExecutor flowableAsyncExecutor = null;

    /**
     * Activation flag of the Flowable job executor
     */
    private boolean enableFlowableJobExecutor = DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR;

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
     * @return the Flowable Engine
     */
    public ProcessEngine getProcessEngine() {
        return this.flowableEngine;
    }

    /**
     * @param eptAndOperation
     *            the end-point Name and operation Name
     * @param flowableService
     *            the Flowable service
     * @return the map with the inserted elements
     */
    public void registerFlowableService(final ServiceEndpointOperationKey eptAndOperation,
            final FlowableService flowableService) {
        this.flowableServices.put(eptAndOperation, flowableService);
    }

    /**
     * @param eptName
     *            the end-point name
     */
    public void removeFlowableService(final String eptName) {

        final Iterator<Entry<ServiceEndpointOperationKey, FlowableService>> itEptOperationToFlowableOperation = this.flowableServices
                .entrySet().iterator();
        while (itEptOperationToFlowableOperation.hasNext()) {
            final Entry<ServiceEndpointOperationKey, FlowableService> entry = itEptOperationToFlowableOperation.next();
            if (entry.getKey().getEndpointName().equals(eptName)) {
                itEptOperationToFlowableOperation.remove();
            }
        }
    }

    /**
     * @param logLevel
     */
    public void logEptOperationToFlowableOperation(final Logger logger, final Level logLevel) {
        if (logger.isLoggable(logLevel)) {
            for (final Map.Entry<ServiceEndpointOperationKey, FlowableService> entry : this.flowableServices
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
     * @return the Flowable Service associated with this end-point name and operation Name
     */
    public FlowableService getFlowableServices(final ServiceEndpointOperationKey eptAndOperation) {
        return this.flowableServices.get(eptAndOperation);
    }

    @Override
    public void doInit() throws JBIException {
        this.getLogger().fine("Start FlowableSE.doInit()");

        try {
            // JDBC Driver
            final String jdbcDriver = FlowableParameterReader
                    .getJdbcDriver(this.getComponentExtensions().get(JDBC_DRIVER), this.getLogger());

            // JDBC URL
            final String jdbcUrlConfigured = this.getComponentExtensions().get(JDBC_URL);
            final String jdbcUrl;
            if (jdbcUrlConfigured == null || jdbcUrlConfigured.trim().isEmpty()) {
                // JDBC URL not set or empty ---> Default value:
                // $PETALS_HOME/data/repository/components/<se-bpmn>/h2-flowable.db
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

            /* TODO Test Flowable database connection configuration */
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
            final String enableFlowableJobExecutorConfigured = this.getComponentExtensions()
                    .get(ENGINE_ENABLE_JOB_EXECUTOR);
            if (enableFlowableJobExecutorConfigured == null || enableFlowableJobExecutorConfigured.trim().isEmpty()) {
                this.getLogger()
                        .info("The activation of the Flowable job executor is not configured. Default value used.");
                this.enableFlowableJobExecutor = DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR;
            } else {
                this.enableFlowableJobExecutor = enableFlowableJobExecutorConfigured.trim().equalsIgnoreCase("false")
                        ? false
                        : (enableFlowableJobExecutorConfigured.trim().equalsIgnoreCase("true") ? true
                                : DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR);
            }

            // Caution:
            // - only the value "false", ignoring case and spaces will disable the BPMN validation,
            // - only the value "true", ignoring case and spaces will enable the BPMN validation,
            // - otherwise, the default value is used.
            final String enableFlowableBpmnValidationConfigured = this.getComponentExtensions()
                    .get(ENGINE_ENABLE_BPMN_VALIDATION);
            final boolean enableFlowableBpmnValidation;
            if (enableFlowableBpmnValidationConfigured == null
                    || enableFlowableBpmnValidationConfigured.trim().isEmpty()) {
                this.getLogger().info(
                        "The activation of the BPMN validation during process deployments is not configured. Default value used.");
                enableFlowableBpmnValidation = DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION;
            } else {
                enableFlowableBpmnValidation = enableFlowableBpmnValidationConfigured.trim().equalsIgnoreCase("false")
                        ? false
                        : (enableFlowableBpmnValidationConfigured.trim().equalsIgnoreCase("true") ? true
                                : DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION);
            }

            ((FlowableSuManager) getServiceUnitManager()).setEnableFlowableBpmnValidation(enableFlowableBpmnValidation);

            final Class<?> identityServiceClass = FlowableParameterReader.getEngineIdentityServiceClassName(
                    this.getComponentExtensions().get(ENGINE_IDENTITY_SERVICE_CLASS_NAME), this.getLogger());

            final File identityServiceCfgFile = FlowableParameterReader.getEngineIdentityServiceConfigurationFile(
                    this.getComponentExtensions().get(ENGINE_IDENTITY_SERVICE_CFG_FILE), this.getLogger());

            this.getLogger().config("Flowable engine configuration:");
            this.getLogger().config("   - " + ENGINE_ENABLE_JOB_EXECUTOR + " = " + this.enableFlowableJobExecutor);
            this.getLogger().config("   - " + ENGINE_ENABLE_BPMN_VALIDATION + " = " + enableFlowableBpmnValidation);
            this.getLogger()
                    .config("   - " + ENGINE_IDENTITY_SERVICE_CLASS_NAME + " = " + identityServiceClass.getName());
            this.getLogger().config("   - " + ENGINE_IDENTITY_SERVICE_CFG_FILE + " = "
                    + (identityServiceCfgFile == null ? "<null>" : identityServiceCfgFile.getAbsolutePath()));

            /* Create an Flowable ProcessEngine with database configuration */
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

            // As recommended by Flowable team, we prefer the Async Job Executor
            pec.setJobExecutorActivate(false);
            // The Async job is enabled ...
            pec.setAsyncExecutorEnabled(this.enableFlowableJobExecutor);
            // ... but must be started when starting the SE
            pec.setAsyncExecutorActivate(false);

            // Override the default configuration of the identity service.
            this.registerIdentityService(pec, identityServiceClass, identityServiceCfgFile);

            // Add post BPMN parse handlers
            this.addPostBpmnParseHandlers(pec);

            this.flowableEngine = pec.buildProcessEngine();
            if (this.enableFlowableJobExecutor) {
                this.flowableAsyncExecutor = pec.getAsyncExecutor();
                this.configureAsyncExecutor();
            } else {
                this.flowableAsyncExecutor = null;
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
            ((Monitoring) this.getMonitoringBean()).setFlowableEngine(this.flowableEngine);

            this.registersIntegrationOperations();

        } catch (final ActivitiException e) {
            throw new JBIException("An error occurred while creating the Flowable BPMN Engine.", e);
        } finally {
            this.getLogger().fine("End FlowableSE.doInit()");
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
                            this.flowableServices.put(
                                    new ServiceEndpointOperationKey(integrationServiceName, integrationEndpointName,
                                            ITG_OP_GETPROCESSINSTANCES),
                                    new GetProcessInstancesOperation(this.flowableEngine.getRuntimeService(),
                                            this.flowableEngine.getHistoryService(),
                                            this.flowableEngine.getRepositoryService(), this.getLogger()));
                            this.flowableServices.put(
                                    new ServiceEndpointOperationKey(integrationServiceName, integrationEndpointName,
                                            ITG_OP_SUSPENDPROCESSINSTANCES),
                                    new SuspendProcessInstancesOperation(this.flowableEngine.getRuntimeService(),
                                            this.getLogger()));
                            this.flowableServices.put(
                                    new ServiceEndpointOperationKey(integrationServiceName, integrationEndpointName,
                                            ITG_OP_ACTIVATEPROCESSINSTANCES),
                                    new ActivateProcessInstancesOperation(this.flowableEngine.getRuntimeService(),
                                            this.getLogger()));
                        } else if (ITG_TASK_PORT_TYPE_NAME.equals(integrationInterfaceName.getLocalPart())) {
                            this.flowableServices.put(
                                    new ServiceEndpointOperationKey(integrationServiceName, integrationEndpointName,
                                            ITG_OP_GETTASKS),
                                    new GetTasksOperation(this.flowableEngine.getTaskService(),
                                            this.flowableEngine.getRepositoryService(), this.getLogger()));
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
     *            The Flowable process engine configuration. Not <code>null</code>.
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
     *            The Flowable process engine configuration. Not <code>null</code>.
     */
    private final void addPostBpmnParseHandlers(final ProcessEngineConfiguration pec) throws JBIException {

        assert pec != null : "pec can not be null";

        if (pec instanceof ProcessEngineConfigurationImpl) {
            final List<BpmnParseHandler> postBpmnParseHandlers = new ArrayList<>();
            postBpmnParseHandlers.add(new ServiceTaskForceAsyncParseHandler());
            ((ProcessEngineConfigurationImpl) pec).setPostBpmnParseHandlers(postBpmnParseHandlers);
        } else {
            this.getLogger().warning(
                    "The implementation of the process engine configuration is not the expected one ! Identity service not overriden !");
        }
    }

    @Override
    public void doStart() throws JBIException {
        this.getLogger().fine("Start FlowableSE.doStart()");

        // Create & Register event listeners
        final RuntimeService runtimeService = this.flowableEngine.getRuntimeService();
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
            // Startup Flowable engine against running states of the SE:
            // - Flowable Engine must be started when the SE is in state 'STOPPED' to be able to deploy process
            // definitions
            // - In state 'STOPPED', the SE will not process incoming requests, so no process instance creation and no
            // user task completion will occur
            // - To avoid the executions of activities trigerred by timer or other events, the Flowable job executor
            // must be stopped when the SE is in state 'STOPPED'
            if (this.enableFlowableJobExecutor) {
                if (this.flowableAsyncExecutor != null) {
                    if (this.flowableAsyncExecutor.isActive()) {
                        this.getLogger().warning("Flowable Job Executor already started !!");
                    } else {
                        this.flowableAsyncExecutor.start();
                        this.configureMonitoringMBeanWithAsyncExecutorThreadPool();
                        this.configureMonitoringMBeanWithDatabaseConnectionPool();
                    }
                } else {
                    this.getLogger().warning("No Flowable Job Executor exists !!");
                }
            } else {
                this.getLogger().info("Flowable Job Executor not started because it is not activated.");
            }

            // TODO: Add JMX operation to start/stop the Flowable job executor when the component is started
            // TODO: Add JMX operation to disable/enable the Flowable job executor when the component is running

        } catch (final ActivitiException e) {
            throw new JBIException("An error occurred while starting the Flowable BPMN Engine.", e);
        } finally {
            this.getLogger().fine("End FlowableSE.doStart()");
        }
    }

    /**
     * COnfigure the asynchronous executor
     */
    private void configureAsyncExecutor() {
        if (this.flowableAsyncExecutor != null) {
            if (this.flowableAsyncExecutor instanceof DefaultAsyncJobExecutor) {

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
                logger.config("   - " + FlowableSEConstants.ENGINE_JOB_EXECUTOR_COREPOOLSIZE + " = " + corePoolSize);
                logger.config("   - " + FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXPOOLSIZE + " = " + maxPoolSize);
                logger.config("   - " + FlowableSEConstants.ENGINE_JOB_EXECUTOR_KEEPALIVETIME + " = " + keepAliveTime);
                logger.config("   - " + FlowableSEConstants.ENGINE_JOB_EXECUTOR_QUEUESIZE + " = " + queueSize);
                logger.config("   - " + FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION + " = "
                        + maxTimerJobsPerAcquisition);
                logger.config("   - " + FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION + " = "
                        + maxAsyncJobsDuePerAcquisition);
                logger.config("   - " + FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME + " = "
                        + asyncJobAcquireWaitTime);
                logger.config("   - " + FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME + " = "
                        + timerJobAcquireWaitTime);
                logger.config("   - " + FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERLOCKTIME + " = " + timerLockTime);
                logger.config(
                        "   - " + FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME + " = " + asyncJobLockTime);

                final DefaultAsyncJobExecutor defaultAsyncJobExecutor = (DefaultAsyncJobExecutor) this.flowableAsyncExecutor;
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

        if (this.flowableAsyncExecutor != null) {
            if (this.flowableAsyncExecutor instanceof DefaultAsyncJobExecutor) {
                final ExecutorService executorService = ((DefaultAsyncJobExecutor) this.flowableAsyncExecutor)
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

        if (this.flowableEngine.getProcessEngineConfiguration() != null) {
            if (this.flowableEngine.getProcessEngineConfiguration() instanceof ProcessEngineConfigurationImpl) {
                final ProcessEngineConfigurationImpl engineConfiguration = (ProcessEngineConfigurationImpl) this.flowableEngine
                        .getProcessEngineConfiguration();
                if (engineConfiguration.getDataSource() == null) {
                    this.getLogger().warning(
                            "No datasource available for the Flowable engine, so no monitoring available on database connection pool !");
                } else if (engineConfiguration.getDataSource() instanceof PooledDataSource) {
                    ((Monitoring) this.getMonitoringBean())
                            .setDataSourcePool((PooledDataSource) engineConfiguration.getDataSource());
                } else {
                    this.getLogger().warning(
                            "The implementation of the Flowable engine datasource is not the expected one, so no monitoring available on database connection pool !");
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
        this.getLogger().fine("Start FlowableSE.doStop()");

        try {
            // Stop the Flowable Job Executor */
            if (this.enableFlowableJobExecutor) {
                if (this.flowableAsyncExecutor != null) {
                    if (this.flowableAsyncExecutor.isActive()) {
                        this.flowableAsyncExecutor.shutdown();
                    } else {
                        this.getLogger().warning("Flowable Job Executor not started !!");
                    }
                } else {
                    this.getLogger().warning("No Flowable Job Executor exists !!");
                }
            } else {
                this.getLogger().info("Flowable Job Executor not stopped because it is not activated.");
            }

        } catch (final ActivitiException e) {
            throw new JBIException("An error occurred while stopping the Flowable BPMN Engine.", e);
        } finally {
            this.getLogger().fine("End FlowableSE.doStop()");
        }

        // Deregister event listeners
        final RuntimeService runtimeService = this.flowableEngine.getRuntimeService();
        runtimeService.removeEventListener(this.processInstanceStartedEventListener);
        runtimeService.removeEventListener(this.processInstanceCompletedEventListener);
        runtimeService.removeEventListener(this.processInstanceCanceledEventListener);
        runtimeService.removeEventListener(this.serviceTaskStartedEventListener);
        runtimeService.removeEventListener(this.userTaskStartedEventListener);
        runtimeService.removeEventListener(this.userTaskCompletedEventListener);

    }

    @Override
    public void doShutdown() throws JBIException {
        this.getLogger().fine("Start FlowableSE.doShutdown()");

        try {
            if (this.flowableEngine != null) {
                this.flowableEngine.close();
            }

        } catch (final ActivitiException e) {
            throw new JBIException("An error occurred while shutdowning the Flowable BPMN Engine.", e);
        } finally {
            this.getLogger().fine("End FlowableSE.doShutdown()");
        }
    }

    @Override
    protected ServiceEngineServiceUnitManager createServiceUnitManager() {
        return new FlowableSuManager(this, this.simpleUUIDGenerator);
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
     * @return The core pool size of the asynchronous job executor of the Flowable engine.
     */
    private int getAsyncJobExecutorCorePoolSize() {
        return this.getParameterAsPositiveInteger(FlowableSEConstants.ENGINE_JOB_EXECUTOR_COREPOOLSIZE,
                FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_COREPOOLSIZE);
    }

    /**
     * @return The max pool size of the asynchronous job executor of the Flowable engine.
     */
    private int getAsyncJobExecutorMaxPoolSize() {
        return this.getParameterAsPositiveInteger(FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXPOOLSIZE,
                FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXPOOLSIZE);
    }

    /**
     * Get the keep-alive time of the asynchronous job executor of the Flowable engine.
     * 
     * @param logger
     * @param configurationExtensions
     *            the component extensions
     * @return the keep alive time of the asynchronous job executor
     */
    private long getAsyncJobExecutorKeepAliveTime() {
        return this.getParameterAsPositiveLong(FlowableSEConstants.ENGINE_JOB_EXECUTOR_KEEPALIVETIME,
                FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_KEEPALIVETIME);
    }

    /**
     * Get the queue size of the asynchronous job executor of the Flowable engine.
     * 
     * @param logger
     * @param configurationExtensions
     *            the component extensions
     * @return the queue size of the asynchronous job executor
     */
    private int getAsyncJobExecutorQueueSize() {
        return this.getParameterAsPositiveInteger(FlowableSEConstants.ENGINE_JOB_EXECUTOR_QUEUESIZE,
                FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_QUEUESIZE);
    }

    /**
     * Get the max number of jobs fetched by query of the asynchronous job executor of the Flowable engine.
     * 
     * @param logger
     * @param configurationExtensions
     *            the component extensions
     * @return the max number of jobs fetched by query of the asynchronous job executor
     */
    private int getAsyncJobExecutorMaxTimerJobsPerAcquisition() {
        return this.getParameterAsPositiveInteger(FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION,
                FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXTIMERJOBSPERACQUISITION);
    }

    /**
     * Get the number of asynchronous jobs due that are fetched by the asynchronous job executor of the Flowable engine.
     * 
     * @param logger
     * @param configurationExtensions
     *            the component extensions
     * @return the number of asynchronous jobs due that are fetched by the asynchronous job executor
     */
    private int getAsyncJobExecutorMaxAsyncJobsDuePerAcquisition() {
        return this.getParameterAsPositiveInteger(FlowableSEConstants.ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION,
                FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_MAXASYNCJOBSDUEPERACQUISITION);
    }

    private int getAsyncJobExecutorAsyncJobAcquireWaitTime() {
        return this.getParameterAsPositiveInteger(FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME,
                FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBACQUIREWAITTIME);
    }

    private int getAsyncJobExecutorTimerJobAcquireWaitTime() {
        return this.getParameterAsPositiveInteger(FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME,
                FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_TIMERJOBACQUIREWAITTIME);
    }

    /**
     * Get the timer job lock time of the asynchronous job executor of the Flowable engine.
     * 
     * @param logger
     * @param configurationExtensions
     *            the component extensions
     * @return the timer job lock time of the asynchronous job executor
     */
    private int getAsyncJobExecutorTimerLockTime() {
        return this.getParameterAsPositiveInteger(FlowableSEConstants.ENGINE_JOB_EXECUTOR_TIMERLOCKTIME,
                FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_TIMERLOCKTIME);
    }

    private int getAsyncJobExecutorAsyncJobLockTime() {
        return this.getParameterAsPositiveInteger(FlowableSEConstants.ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME,
                FlowableSEConstants.DEFAULT_ENGINE_JOB_EXECUTOR_ASYNCJOBLOCKTIME);
    }
}