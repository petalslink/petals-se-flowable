/**
 * Copyright (c) 2014-2019 Linagora
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
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_ASYNC_FAILED_JOB_WAIT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_DEFAULT_FAILED_JOB_WAIT_TIME;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_ENABLE_BPMN_VALIDATION;
import static org.ow2.petals.flowable.FlowableSEConstants.ENGINE_ENABLE_JOB_EXECUTOR;
import static org.ow2.petals.flowable.FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CFG_FILE;
import static org.ow2.petals.flowable.FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CLASS_NAME;
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
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_GROUP_PORT_TYPE_NAME;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_ACTIVATEPROCESSINSTANCES;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETPROCESSINSTANCES;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETTASKS;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETUSER;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_SEARCHGROUPS;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_SEARCHUSERS;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_SUSPENDPROCESSINSTANCES;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_PROCESSINSTANCES_PORT_TYPE_NAME;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_PORT_TYPE_NAME;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_USER_PORT_TYPE_NAME;

import java.io.File;
import java.net.InetSocketAddress;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumSet;
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
import javax.servlet.DispatcherType;
import javax.servlet.FilterRegistration;
import javax.xml.namespace.QName;

import org.apache.cxf.Bus;
import org.apache.cxf.BusFactory;
import org.apache.cxf.transport.ConduitInitiatorManager;
import org.apache.ibatis.datasource.pooled.PooledDataSource;
import org.eclipse.jetty.security.IdentityService;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;
import org.flowable.engine.HistoryService;
import org.flowable.engine.ProcessEngine;
import org.flowable.engine.ProcessEngineConfiguration;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.TaskService;
import org.flowable.engine.common.api.FlowableException;
import org.flowable.engine.impl.bpmn.parser.factory.XMLImporterFactory;
import org.flowable.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.flowable.engine.parse.BpmnParseHandler;
import org.flowable.job.service.impl.asyncexecutor.AsyncExecutor;
import org.flowable.job.service.impl.asyncexecutor.DefaultAsyncJobExecutor;
import org.ow2.easywsdl.wsdl.api.Endpoint;
import org.ow2.easywsdl.wsdl.api.WSDLException;
import org.ow2.petals.basisapi.exception.PetalsException;
import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;
import org.ow2.petals.component.framework.listener.AbstractListener;
import org.ow2.petals.component.framework.mbean.MBeanHelper;
import org.ow2.petals.component.framework.se.AbstractServiceEngine;
import org.ow2.petals.component.framework.se.ServiceEngineServiceUnitManager;
import org.ow2.petals.component.framework.util.ServiceEndpointOperationKey;
import org.ow2.petals.component.framework.util.WSDLUtilImpl;
import org.ow2.petals.flowable.admin.AdminOperations;
import org.ow2.petals.flowable.event.AbstractEventListener;
import org.ow2.petals.flowable.event.CallActivityStartedEventListener;
import org.ow2.petals.flowable.event.IntermediateCatchMessageEventEndedEventListener;
import org.ow2.petals.flowable.event.IntermediateCatchMessageEventStartedEventListener;
import org.ow2.petals.flowable.event.ProcessInstanceCanceledEventListener;
import org.ow2.petals.flowable.event.ProcessInstanceCompletedEventListener;
import org.ow2.petals.flowable.event.ProcessInstanceStartedEventListener;
import org.ow2.petals.flowable.event.ServiceTaskStartedEventListener;
import org.ow2.petals.flowable.event.UserTaskCompletedEventListener;
import org.ow2.petals.flowable.event.UserTaskStartedEventListener;
import org.ow2.petals.flowable.identity.SeFlowableIdmServiceConfigurator;
import org.ow2.petals.flowable.incoming.FlowableService;
import org.ow2.petals.flowable.incoming.integration.ActivateProcessInstancesOperation;
import org.ow2.petals.flowable.incoming.integration.GetProcessInstancesOperation;
import org.ow2.petals.flowable.incoming.integration.GetTasksOperation;
import org.ow2.petals.flowable.incoming.integration.GetUserOperation;
import org.ow2.petals.flowable.incoming.integration.SearchGroupsOperation;
import org.ow2.petals.flowable.incoming.integration.SearchUsersOperation;
import org.ow2.petals.flowable.incoming.integration.SuspendProcessInstancesOperation;
import org.ow2.petals.flowable.incoming.integration.exception.OperationInitializationException;
import org.ow2.petals.flowable.monitoring.Monitoring;
import org.ow2.petals.flowable.outgoing.PetalsSender;
import org.ow2.petals.flowable.outgoing.WSDLImporterForFlowableFactory;
import org.ow2.petals.flowable.outgoing.cxf.transport.PetalsCxfTransportFactory;
import org.ow2.petals.flowable.rest.FlowableProcessApiConfiguration;
import org.ow2.petals.flowable.rest.config.SecurityConfiguration;
import org.ow2.petals.probes.api.exceptions.MultipleProbesFactoriesFoundException;
import org.ow2.petals.probes.api.exceptions.NoProbesFactoryFoundException;
import org.ow2.petals.se.flowable.clientserver.api.admin.AdminRuntimeService;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.AnnotationConfigWebApplicationContext;
import org.springframework.web.filter.DelegatingFilterProxy;
import org.springframework.web.servlet.DispatcherServlet;

import com.ebmwebsourcing.easycommons.uuid.SimpleUUIDGenerator;

/**
 * The component class of the Flowable BPMN Service Engine.
 * 
 * @author Bertrand Escudie - Linagora
 */
public class FlowableSE extends AbstractServiceEngine implements AdminRuntimeService {

    /**
     * The Flowable BPMN Engine.
     */
    private ProcessEngine flowableEngine;

    private Server restServer;

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
     * Event listener fired when a call activity is started
     */
    private AbstractEventListener callActivityStartedEventListener;

    /**
     * Event listener fired when a intermediate catch message event is started
     */
    private AbstractEventListener intermediateCatchMessageEventStartedEventListener;

    /**
     * Event listener fired when a intermediate catch message event is ended
     */
    private AbstractEventListener intermediateCatchMessageEventEndedEventListener;

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
            final ProcessEngineConfiguration pec = ProcessEngineConfiguration
                    .createStandaloneProcessEngineConfiguration();

            this.configureFlowableEngineDatabase(pec);
            this.configureFlowableEngineOtherParams(pec);
            this.configureFlowableIdmEngine(pec);

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

            // We register the Petals transport into Apache CXF
            this.registerCxfPetalsTransport();

            // The Async job is must be started when starting the SE according to the configuration
            // 'enableFlowableJobExecutor'.
            // We force its activation to false to avoid an automatic startup when starting the Flowable engine. The
            // right activation status will be set in Flowable engine configuration once the Flowable engine will be
            // started.
            pec.setAsyncExecutorActivate(false);

            // Add post BPMN parse handlers
            this.addPostBpmnParseHandlers(pec);

            // Async executor configuration
            this.configureAsyncExecutor(pec);

            final ProcessEngineConfigurationImpl pecImpl;
            if (pec instanceof ProcessEngineConfigurationImpl) {
                pecImpl = (ProcessEngineConfigurationImpl) pec;

                // We register our WSDL importer to be able to resolve import against a source system id based on our
                // internal URI
                final XMLImporterFactory wsdlImporterForFlowableFactory = new WSDLImporterForFlowableFactory(
                        this.getServiceUnitManager());
                pecImpl.setWsdlImporterFactory(wsdlImporterForFlowableFactory);
            } else {
                pecImpl = null;
                this.getLogger().warning(
                        "The implementation of the process engine configuration is not the expected one ! No Petals services can be invoked !");
            }

            // Create the Flowable engine
            this.flowableEngine = pec.buildProcessEngine();

            // Configure async executor
            if (this.enableFlowableJobExecutor) {
                this.flowableAsyncExecutor = pec.getAsyncExecutor();
            } else {
                this.flowableAsyncExecutor = null;
            }

            // Caution: Configuration beans are initialized when building the process engine
            if (pecImpl != null) {
                // We add to the BPMN engine the bean in charge of sending Petals message exchange
                final AbstractListener petalsSender = new PetalsSender(this);
                pecImpl.getBeans().put(PETALS_SENDER_COMP_NAME, petalsSender);
            } else {
                this.getLogger().warning("Capability to invoke Petals services not activated !");
            }

            // Configure a part of the monitoring MBean. Another part is configured on component startup.
            ((Monitoring) this.getMonitoringBean()).setFlowableEngine(this.flowableEngine);

            this.registersIntegrationOperations();

            if (this.getRestApiEnable()) {
                this.createRestApi();
            } else {
                this.getLogger().config("Flowable REST API configuration: disabled");
            }
        } catch (final Exception e) {
            throw new JBIException("An error occurred while creating the Flowable BPMN Engine.", e);
        } finally {
            this.getLogger().fine("End FlowableSE.doInit()");
        }
    }

    private void createRestApi() throws Exception {
        // contains the singleton, it needs to be a parent of the application context to work...
        final AnnotationConfigWebApplicationContext rootContext = new AnnotationConfigWebApplicationContext();
        rootContext.refresh();
        rootContext.getBeanFactory().registerSingleton(
                FlowableProcessApiConfiguration.FLOWABLE_REST_PROCESS_ENGINE_QUALIFIER, this.flowableEngine);
        rootContext.getBeanFactory().registerSingleton(SecurityConfiguration.FLOWABLE_REST_API_ACCESS_GROUP_QUALIFIER,
                getRestApiAccessGroup());

        final AnnotationConfigWebApplicationContext applicationContext = new AnnotationConfigWebApplicationContext();
        applicationContext.setParent(rootContext);

        final ServletContextHandler context = new ServletContextHandler();
        context.setContextPath("/flowable-rest-api");
        final ServletHolder servletHolder = new ServletHolder(new DispatcherServlet(applicationContext));
        servletHolder.setAsyncSupported(true);
        context.addServlet(servletHolder, "/*");

        this.getLogger().config("Flowable REST API configuration: enabled");
        this.getLogger().config("   - address: " + this.getRestApiAddress());
        this.getLogger().config("   - port. " + this.getRestApiPort());
        this.getLogger().config("   - access group: " + this.getRestApiAccessGroup());

        final FilterRegistration.Dynamic security = context.getServletContext().addFilter("springSecurityFilterChain",
                new DelegatingFilterProxy());
        security.addMappingForUrlPatterns(
                EnumSet.of(DispatcherType.REQUEST, DispatcherType.FORWARD, DispatcherType.ASYNC), false, "/*");
        security.setAsyncSupported(true);
        context.getServletContext().setAttribute(WebApplicationContext.ROOT_WEB_APPLICATION_CONTEXT_ATTRIBUTE,
                applicationContext);

        applicationContext.setServletContext(context.getServletContext());
        applicationContext.register(FlowableProcessApiConfiguration.class);
        applicationContext.refresh();

        this.restServer = new Server(new InetSocketAddress(this.getRestApiAddress(), this.getRestApiPort()));
        restServer.setHandler(context);
        restServer.start();
    }

    private void configureFlowableIdmEngine(final ProcessEngineConfiguration pec) throws JBIException {

        final Class<?> idmEngineConfiguratorClass = FlowableParameterReader.getIdmEngineConfiguratorClassName(
                this.getComponentExtensions().get(IDM_ENGINE_CONFIGURATOR_CLASS_NAME), this.getLogger());

        final File idmEngineConfiguratorCfgFile = FlowableParameterReader.getEngineIdentityServiceConfigurationFile(
                this.getComponentExtensions().get(IDM_ENGINE_CONFIGURATOR_CFG_FILE), this.getLogger());

        this.getLogger().config("Flowable IDM engine configuration:");
        this.getLogger()
                .config("   - " + IDM_ENGINE_CONFIGURATOR_CLASS_NAME + " = " + idmEngineConfiguratorClass.getName());
        this.getLogger().config("   - " + IDM_ENGINE_CONFIGURATOR_CFG_FILE + " = "
                + (idmEngineConfiguratorCfgFile == null ? "<null>" : idmEngineConfiguratorCfgFile.getAbsolutePath()));

        // Override the default configuration of the identity service.
        this.registerIdentityService(pec, idmEngineConfiguratorClass, idmEngineConfiguratorCfgFile);
    }

    private void configureFlowableEngineOtherParams(final ProcessEngineConfiguration pec) {

        // Caution:
        // - only the value "false", ignoring case and spaces will disable the BPMN validation,
        // - only the value "true", ignoring case and spaces will enable the BPMN validation,
        // - otherwise, the default value is used.
        final String enableFlowableBpmnValidationConfigured = this.getComponentExtensions()
                .get(ENGINE_ENABLE_BPMN_VALIDATION);
        final boolean enableFlowableBpmnValidation;
        if (enableFlowableBpmnValidationConfigured == null || enableFlowableBpmnValidationConfigured.trim().isEmpty()) {
            this.getLogger().info(
                    "The activation of the BPMN validation during process deployments is not configured. Default value used.");
            enableFlowableBpmnValidation = DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION;
        } else {
            enableFlowableBpmnValidation = enableFlowableBpmnValidationConfigured.trim().equalsIgnoreCase("false")
                    ? false
                    : (enableFlowableBpmnValidationConfigured.trim().equalsIgnoreCase("true") ? true
                            : DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION);
        }

        final int defaultFailedJobWaitTime = this.getDefaultFailedJobWaitTime();
        final int asyncFailedJobWaitTime = this.getAsyncFailedJobWaitTime();

        this.getLogger().config("Flowable Engine other params configuration:");
        this.getLogger().config("   - " + ENGINE_ENABLE_BPMN_VALIDATION + " = " + enableFlowableBpmnValidation);
        this.getLogger().config("   - " + ENGINE_DEFAULT_FAILED_JOB_WAIT_TIME + " = " + defaultFailedJobWaitTime);
        this.getLogger().config("   - " + ENGINE_ASYNC_FAILED_JOB_WAIT_TIME + " = " + asyncFailedJobWaitTime);

        ((FlowableSuManager) getServiceUnitManager()).setEnableFlowableBpmnValidation(enableFlowableBpmnValidation);
        pec.setAsyncFailedJobWaitTime(asyncFailedJobWaitTime);
        pec.setDefaultFailedJobWaitTime(defaultFailedJobWaitTime);

    }

    private void configureFlowableEngineDatabase(final ProcessEngineConfiguration pec) throws JBIException {
        // JDBC Driver
        final String jdbcDriver = FlowableParameterReader.getJdbcDriver(this.getComponentExtensions().get(JDBC_DRIVER),
                this.getLogger());

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
                this.getLogger()
                        .warning("Invalid value for the number of JDBC Max Active Connections. Default value used.");
                jdbcMaxActiveConnections = DEFAULT_JDBC_MAX_ACTIVE_CONNECTIONS;
            }
        }

        final String jdbcMaxIdleConnectionsConfigured = this.getComponentExtensions().get(JDBC_MAX_IDLE_CONNECTIONS);
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
                this.getLogger().warning("Invalid value for the number of JDBC Max Checkout Time. Default value used.");
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

        this.getLogger().config("Flowable Engine database configuration:");
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

    }

    /**
     * Registers integration operations
     */
    private void registersIntegrationOperations() {

        // Register integration operation
        final List<Endpoint> integrationEndpoints = WSDLUtilImpl.getEndpointList(this.getNativeWsdl().getDescription());
        if (!integrationEndpoints.isEmpty()) {
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
                        } else if (ITG_USER_PORT_TYPE_NAME.equals(integrationInterfaceName.getLocalPart())) {
                            this.flowableServices.put(
                                    new ServiceEndpointOperationKey(integrationServiceName, integrationEndpointName,
                                            ITG_OP_GETUSER),
                                    new GetUserOperation(this.flowableEngine.getIdentityService(), this.getLogger()));
                            this.flowableServices.put(
                                    new ServiceEndpointOperationKey(integrationServiceName, integrationEndpointName,
                                            ITG_OP_SEARCHUSERS),
                                    new SearchUsersOperation(this.flowableEngine.getIdentityService(),
                                            this.getLogger()));
                        } else if (ITG_GROUP_PORT_TYPE_NAME.equals(integrationInterfaceName.getLocalPart())) {
                            this.flowableServices.put(
                                    new ServiceEndpointOperationKey(integrationServiceName, integrationEndpointName,
                                            ITG_OP_SEARCHGROUPS),
                                    new SearchGroupsOperation(this.flowableEngine.getIdentityService(),
                                            this.getLogger()));
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
     * Initialize the identity management engine
     * 
     * @param pec
     *            The Flowable process engine configuration. Not <code>null</code>.
     * @param idmEngineConfiguratorClass
     *            The identity service class defining which identity management engine will be used. Not
     *            <code>null</code>. Must implement {@link IdentityService}.
     * @param idmCfgFile
     *            The identity management engine configuration file. If <code>null</code>, the default configuration of
     *            the identity management engine will be used.
     */
    private final void registerIdentityService(final ProcessEngineConfiguration pec,
            final Class<?> idmEngineConfiguratorClass, final File idmCfgFile) throws JBIException {

        assert pec != null : "pec can not be null";
        assert idmEngineConfiguratorClass != null : "idmEngineConfiguratorClass can not be null";
        assert SeFlowableIdmServiceConfigurator.class.isAssignableFrom(
                idmEngineConfiguratorClass) : "The IDM engine configurator service class defining which IDM engine will be used does not implement AbstractProcessEngineConfigurator";

        final Object idmEngineConfiguratorObj;
        try {
            idmEngineConfiguratorObj = idmEngineConfiguratorClass.newInstance();
            assert idmEngineConfiguratorObj instanceof SeFlowableIdmServiceConfigurator;
            final SeFlowableIdmServiceConfigurator idmEngineConfigurator = (SeFlowableIdmServiceConfigurator) idmEngineConfiguratorObj;

            if (pec instanceof ProcessEngineConfigurationImpl) {
                ((ProcessEngineConfigurationImpl) pec).setDisableIdmEngine(false);
                idmEngineConfigurator.setConfigurationFile(idmCfgFile);
                idmEngineConfigurator.setLogger(this.getLogger());
                ((ProcessEngineConfigurationImpl) pec).setIdmEngineConfigurator(idmEngineConfigurator);
            } else {
                this.getLogger().warning(
                        "The implementation of the process engine configuration is not the expected one ! Identity management engine not overriden !");
            }
        } catch (final InstantiationException | IllegalAccessException e) {
            throw new JBIException("An error occurred while instantiating the identity management engine.", e);
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
            postBpmnParseHandlers.add(new ServiceTaskForceAsyncParseHandler(this.getLogger()));
            postBpmnParseHandlers.add(new CallActivityForceAsyncParseHandler(this.getLogger()));
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

        final HistoryService historyService = this.flowableEngine.getHistoryService();
        this.processInstanceCompletedEventListener = new ProcessInstanceCompletedEventListener(historyService,
                this.getLogger());
        runtimeService.addEventListener(this.processInstanceCompletedEventListener,
                this.processInstanceCompletedEventListener.getListenEventType());

        this.processInstanceCanceledEventListener = new ProcessInstanceCanceledEventListener(historyService,
                this.getLogger());
        runtimeService.addEventListener(this.processInstanceCanceledEventListener,
                this.processInstanceCanceledEventListener.getListenEventType());

        this.serviceTaskStartedEventListener = new ServiceTaskStartedEventListener(runtimeService, this.getLogger());
        runtimeService.addEventListener(this.serviceTaskStartedEventListener,
                this.serviceTaskStartedEventListener.getListenEventType());

        final TaskService taskService = this.flowableEngine.getTaskService();
        this.userTaskStartedEventListener = new UserTaskStartedEventListener(this.simpleUUIDGenerator, taskService,
                this.getLogger());
        runtimeService.addEventListener(this.userTaskStartedEventListener,
                this.userTaskStartedEventListener.getListenEventType());

        this.userTaskCompletedEventListener = new UserTaskCompletedEventListener(taskService, this.getLogger());
        runtimeService.addEventListener(this.userTaskCompletedEventListener,
                this.userTaskCompletedEventListener.getListenEventType());

        this.callActivityStartedEventListener = new CallActivityStartedEventListener(runtimeService,
                this.simpleUUIDGenerator, this.getLogger());
        runtimeService.addEventListener(this.callActivityStartedEventListener,
                this.callActivityStartedEventListener.getListenEventType());

        this.intermediateCatchMessageEventStartedEventListener = new IntermediateCatchMessageEventStartedEventListener(
                this.simpleUUIDGenerator, runtimeService, this.getLogger());
        runtimeService.addEventListener(this.intermediateCatchMessageEventStartedEventListener,
                this.intermediateCatchMessageEventStartedEventListener.getListenEventType());

        this.intermediateCatchMessageEventEndedEventListener = new IntermediateCatchMessageEventEndedEventListener(
                runtimeService, this.getLogger());
        runtimeService.addEventListener(this.intermediateCatchMessageEventEndedEventListener,
                this.intermediateCatchMessageEventEndedEventListener.getListenEventType());

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
                    }
                    this.configureMonitoringMBeanWithAsyncExecutorThreadPool();
                    this.configureMonitoringMBeanWithDatabaseConnectionPool();
                } else {
                    this.getLogger().warning("No Flowable Job Executor exists !!");
                }
            } else {
                this.getLogger().info("Flowable Job Executor not started because it is not activated.");
            }

            // TODO: Add JMX operation to start/stop the Flowable job executor when the component is started
            // TODO: Add JMX operation to disable/enable the Flowable job executor when the component is running

        } catch (final FlowableException e) {
            throw new JBIException("An error occurred while starting the Flowable BPMN Engine.", e);
        } finally {
            this.getLogger().fine("End FlowableSE.doStart()");
        }
    }

    /**
     * Configure the asynchronous executor
     */
    private void configureAsyncExecutor(final ProcessEngineConfiguration pec) {

        final Logger logger = this.getLogger();
        logger.config("Asynchronous job executor configuration:");
        logger.config("   - " + ENGINE_ENABLE_JOB_EXECUTOR + " = " + this.enableFlowableJobExecutor);
        if (this.enableFlowableJobExecutor) {

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

            final DefaultAsyncJobExecutor defaultAsyncJobExecutor = new DefaultAsyncJobExecutor();
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

            // Startup and shutdown of the async executor is controlled through startup and stop of the SE Flowable
            pec.setAsyncExecutorActivate(false);

            pec.setAsyncExecutor(defaultAsyncJobExecutor);

        } else {
            pec.setAsyncExecutorActivate(false);
            pec.setAsyncExecutor(null);
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

        } catch (final FlowableException e) {
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
        runtimeService.removeEventListener(this.callActivityStartedEventListener);
        runtimeService.removeEventListener(this.intermediateCatchMessageEventStartedEventListener);
        runtimeService.removeEventListener(this.intermediateCatchMessageEventEndedEventListener);

    }

    @Override
    public void doShutdown() throws JBIException {
        this.getLogger().fine("Start FlowableSE.doShutdown()");

        try {
            final PEtALSCDKException exception = new PEtALSCDKException("Error stopping the component");

            try {
                if (this.restServer != null) {
                    this.restServer.stop();
                }
            } catch (final Exception e) {
                exception.addSuppressed(e);
            }

            try {
                if (this.flowableEngine != null) {
                    this.flowableEngine.close();
                }
            } catch (final Exception e) {
                exception.addSuppressed(e);
            }

            exception.throwIfNeeded();
        } finally {
            this.getLogger().fine("End FlowableSE.doShutdown()");
        }
    }

    @Override
    protected ServiceEngineServiceUnitManager createServiceUnitManager() {
        return new FlowableSuManager(this, this.simpleUUIDGenerator);
    }

    @Override
    public Collection<String> getMBeanOperationsNames() {
        final Collection<String> methods = super.getMBeanOperationsNames();

        methods.addAll(MBeanHelper.getMethodsNames(AdminRuntimeService.class));

        return methods;
    }

    private void registerCxfPetalsTransport() {
        final Bus bus = BusFactory.getThreadDefaultBus();
        final PetalsCxfTransportFactory cxfPetalsTransport = new PetalsCxfTransportFactory();
        final ConduitInitiatorManager extension = bus.getExtension(ConduitInitiatorManager.class);
        extension.registerConduitInitiator(PetalsCxfTransportFactory.TRANSPORT_ID, cxfPetalsTransport);
    }

    @Override
    protected org.ow2.petals.component.framework.monitoring.Monitoring createMonitoringMBean()
            throws MultipleProbesFactoriesFoundException, NoProbesFactoryFoundException {

        return new Monitoring(this.getProbesTimer(), this.getResponseTimeProbeSamplePeriod());
    }

    /**
     * @return The async failed job wait time of the Flowable engine.
     */
    private int getAsyncFailedJobWaitTime() {
        return this.getParameterAsPositiveInteger(ENGINE_ASYNC_FAILED_JOB_WAIT_TIME,
                FlowableSEConstants.DEFAULT_ASYNC_FAILED_JOB_WAIT_TIME);
    }

    /**
     * @return The default failed job wait time of the Flowable engine.
     */
    private int getDefaultFailedJobWaitTime() {
        return this.getParameterAsPositiveInteger(ENGINE_DEFAULT_FAILED_JOB_WAIT_TIME,
                FlowableSEConstants.DEFAULT_DEFAULT_FAILED_JOB_WAIT_TIME);
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

    private int getRestApiPort() {
        return this.getParameterAsInteger(FlowableSEConstants.ENGINE_REST_API_PORT,
                FlowableSEConstants.DEFAULT_ENGINE_REST_API_PORT);
    }

    private String getRestApiAccessGroup() {
        return this.getParameterAsNotEmptyTrimmedString(FlowableSEConstants.ENGINE_REST_API_ACCESS_GROUP,
                FlowableSEConstants.DEFAULT_ENGINE_REST_API_ACCESS_GROUP);
    }

    private boolean getRestApiEnable() {
        return this.getParameterAsBoolean(FlowableSEConstants.ENGINE_REST_API_ENABLE,
                FlowableSEConstants.DEFAULT_ENGINE_REST_API_ENABLE);
    }

    private String getRestApiAddress() {
        return this.getParameterAsNotEmptyTrimmedString(FlowableSEConstants.ENGINE_REST_API_ADDRESS,
                FlowableSEConstants.DEFAULT_ENGINE_REST_API_ADDRESS);
    }

    @Override
    public List<String> listPurgeableProcessInstances(final String processDefinitionKey,
            final int processDefinitionVersion)
            throws PetalsException {

        return AdminOperations.listPurgeableProcessInstances(processDefinitionKey, processDefinitionVersion,
                this.flowableEngine);
    }

    @Override
    public List<String> purgeProcessInstance(final String procInstId, final boolean returnsCorrelatedFlows)
            throws PetalsException {

        return AdminOperations.purgeProcessInstance(procInstId, returnsCorrelatedFlows, this.flowableEngine);
    }

    @Override
    public void undeployProcessDefinition(final String procDefKey, final int procDefVer) throws PetalsException {

        AdminOperations.undeployProcessDefinition(procDefKey, procDefVer, this.flowableEngine);

    }
}
