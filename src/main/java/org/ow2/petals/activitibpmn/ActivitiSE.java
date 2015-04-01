/**
 * Copyright (c) 2014-2015 Linagora
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

import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_MONIT_TRACE_DELAY;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_SCHEDULED_LOGGER_CORE_SIZE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.MONIT_TRACE_DELAY;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.SCHEDULED_LOGGER_CORE_SIZE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.Activiti.PETALS_SENDER_COMP_NAME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DATABASE_SCHEMA_UPDATE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DATABASE_TYPE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_JDBC_DRIVER;
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
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_OP_GETTASKS;

import java.io.File;
import java.net.MalformedURLException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;
import javax.xml.namespace.QName;

import org.activiti.engine.ActivitiException;
import org.activiti.engine.ProcessEngine;
import org.activiti.engine.ProcessEngineConfiguration;
import org.activiti.engine.delegate.event.ActivitiEventType;
import org.activiti.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.apache.cxf.Bus;
import org.apache.cxf.BusFactory;
import org.apache.cxf.transport.ConduitInitiatorManager;
import org.ow2.easywsdl.wsdl.api.Endpoint;
import org.ow2.easywsdl.wsdl.api.WSDLException;
import org.ow2.petals.activitibpmn.event.ProcessCanceledEventListener;
import org.ow2.petals.activitibpmn.event.ProcessCompletedEventListener;
import org.ow2.petals.activitibpmn.incoming.ActivitiService;
import org.ow2.petals.activitibpmn.incoming.integration.GetTasksOperation;
import org.ow2.petals.activitibpmn.incoming.integration.exception.OperationInitializationException;
import org.ow2.petals.activitibpmn.outgoing.PetalsSender;
import org.ow2.petals.activitibpmn.outgoing.cxf.transport.PetalsCxfTransportFactory;
import org.ow2.petals.component.framework.listener.AbstractListener;
import org.ow2.petals.component.framework.se.AbstractServiceEngine;
import org.ow2.petals.component.framework.su.AbstractServiceUnitManager;
import org.ow2.petals.component.framework.util.EndpointOperationKey;
import org.ow2.petals.component.framework.util.WSDLUtilImpl;


/**
 * The component class of the Activiti BPMN Service Engine.
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
    private final Map<EndpointOperationKey, ActivitiService> activitiServices = new ConcurrentHashMap<EndpointOperationKey, ActivitiService>();

    /**
     * An executor service to log MONIT trace about end of process instances
     */
    private ScheduledExecutorService scheduledLogger = null;

    /**
     * Delay to wait before to log some MONIT traces
     */
    // TODO: monitTraceDelay should be hot-changed
    private int monitTraceDelay = DEFAULT_MONIT_TRACE_DELAY;

    /**
     * Core size of the thread pool in charge of logging delayed MONIT traces
     */
    // TODO: scheduledLoggerCoreSize should be hot-changed
    private int scheduledLoggerCoreSize = DEFAULT_SCHEDULED_LOGGER_CORE_SIZE;

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
    public void registerActivitiService(final EndpointOperationKey eptAndOperation,
            final ActivitiService activitiservice) {
        this.activitiServices.put(eptAndOperation, activitiservice);
    }

    /**
     * @param eptName
     *            the end-point name
     */
    public void removeActivitiService(final String eptName) {

        final Iterator<Entry<EndpointOperationKey, ActivitiService>> itEptOperationToActivitiOperation = this.activitiServices
                .entrySet().iterator();
        while (itEptOperationToActivitiOperation.hasNext()) {
            final Entry<EndpointOperationKey, ActivitiService> entry = itEptOperationToActivitiOperation.next();
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
            for (final Map.Entry<EndpointOperationKey, ActivitiService> entry : this.activitiServices.entrySet()) {
                final EndpointOperationKey key = entry.getKey();
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
    public ActivitiService getActivitiServices(final EndpointOperationKey eptAndOperation) {
        return this.activitiServices.get(eptAndOperation);
	}
	
	
    @Override
	public void doInit() throws JBIException {
        this.getLogger().fine("Start ActivitiSE.doInit()");

		try {
            // JDBC Driver
            final String jdbcDriverConfigured = this.getComponentExtensions().get(JDBC_DRIVER);
            final String jdbcDriver;
            if (jdbcDriverConfigured == null || jdbcDriverConfigured.trim().isEmpty()) {
                this.getLogger().info("No JDBC Driver configured for database. Default value used.");
                jdbcDriver = DEFAULT_JDBC_DRIVER;
            } else {
                jdbcDriver = jdbcDriverConfigured;
            }

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

            final String jdbcMaxActiveConnectionsConfigured = this.getComponentExtensions().get(
                    JDBC_MAX_ACTIVE_CONNECTIONS);
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
                    this.getLogger().warning(
                            "Invalid value for the number of JDBC Max Idle Connections. Default value used.");
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
                    this.getLogger().warning(
                            "Invalid value for the number of JDBC Max Checkout Time. Default value used.");
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
            final String databaseSchemaUpdate = this.getComponentExtensions().get(DATABASE_SCHEMA_UPDATE);

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

            final String monitTraceDelayConfigured = this.getComponentExtensions().get(MONIT_TRACE_DELAY);
            if (monitTraceDelayConfigured == null || monitTraceDelayConfigured.trim().isEmpty()) {
                this.getLogger().info("No MONIT trace delay configured. Default value used.");
                this.monitTraceDelay = DEFAULT_MONIT_TRACE_DELAY;
            } else {
                try {
                    this.monitTraceDelay = Integer.parseInt(monitTraceDelayConfigured);
                } catch (final NumberFormatException e) {
                    this.getLogger().warning("Invalid value for the MONIT trace delay. Default value used.");
                    this.monitTraceDelay = DEFAULT_MONIT_TRACE_DELAY;
                }
            }

            final String scheduledLoggerCoreSizeConfigured = this.getComponentExtensions().get(
                    SCHEDULED_LOGGER_CORE_SIZE);
            if (scheduledLoggerCoreSizeConfigured == null || scheduledLoggerCoreSizeConfigured.trim().isEmpty()) {
                this.getLogger()
                        .info("No core size of the thread pool in charge of logging MONIT traces is configured. Default value used.");
                this.scheduledLoggerCoreSize = DEFAULT_SCHEDULED_LOGGER_CORE_SIZE;
            } else {
                try {
                    this.scheduledLoggerCoreSize = Integer.parseInt(scheduledLoggerCoreSizeConfigured);
                } catch (final NumberFormatException e) {
                    this.getLogger()
                            .warning(
                                    "Invalid value for the core size of the thread pool in charge of logging MONIT traces. Default value used.");
                    this.scheduledLoggerCoreSize = DEFAULT_SCHEDULED_LOGGER_CORE_SIZE;
                }
            }

            this.getLogger().config("Other configuration parameters:");
            this.getLogger().config("   - " + MONIT_TRACE_DELAY + " = " + this.monitTraceDelay);
            this.getLogger().config("   - " + SCHEDULED_LOGGER_CORE_SIZE + " = " + this.scheduledLoggerCoreSize);
		    
	        /* TODO Test Activiti database connection configuration */
	        /* TODO Test the Database Schema Version
	         *      What about databaseSchemaUpdate values "true" and "create-drop" 
	         */
	        /* TODO Set the non set value with default value */

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
            pec.setDatabaseSchemaUpdate(databaseSchemaUpdate);
            pec.setJobExecutorActivate(false);

            // We register the Petals transport into Apache CXF
            this.registerCxfPetalsTransport();

            this.activitiEngine = pec.buildProcessEngine();
            

            // Caution: Beans of the configuration are initialized when building the process engine
            if (pec instanceof ProcessEngineConfigurationImpl) {
                // We add to the BPMN engine the bean in charge of sending Petals message exchange
                final AbstractListener petalsSender = new PetalsSender();
                petalsSender.init(this);
                ((ProcessEngineConfigurationImpl) pec).getBeans().put(PETALS_SENDER_COMP_NAME, petalsSender);
            } else {
                this.getLogger().warning("The implementation of the process engine configuration is not the expected one ! No Petals services can be invoked !");
            }

            // Register integration operation
            final List<Endpoint> integrationEndpoints = WSDLUtilImpl.getEndpointList(this.getNativeWsdl()
                    .getDescription());
            if (integrationEndpoints.size() > 1) {
                throw new JBIException("Unexpected endpoint number supporting integration services");
            } else if (integrationEndpoints.size() == 1) {
                try {
                    final Endpoint endpoint = integrationEndpoints.get(0);
                    final String integrationEndpointName = endpoint.getName();
                    final QName integrationInterfaceName = endpoint.getService().getInterface().getQName();
                    this.activitiServices
                            .put(new EndpointOperationKey(integrationEndpointName, integrationInterfaceName,
                                    ITG_OP_GETTASKS),
                            new GetTasksOperation(this.activitiEngine.getTaskService(), this.activitiEngine
                                    .getRepositoryService(), this.getLogger()));
                } catch (final OperationInitializationException | WSDLException e) {
                    this.getLogger().log(Level.WARNING, "Integration operations are not completly initialized", e);
                }
            } else {
                this.getLogger().warning("No endpoint exists to execute integration operations");
            }

		} catch( final ActivitiException e ) {
			throw new JBIException( "An error occurred while creating the Activiti BPMN Engine.", e );
        } finally {
            this.getLogger().fine("End ActivitiSE.doInit()");
		}
	}


    @Override
	public void doStart() throws JBIException {
        this.getLogger().fine("Start ActivitiSE.doStart()");

        this.scheduledLogger = Executors.newScheduledThreadPool(this.scheduledLoggerCoreSize,
                new ScheduledLoggerThreadFactory(ActivitiSE.this.getContext().getComponentName()));

        this.activitiEngine.getRuntimeService().addEventListener(
                        new ProcessCompletedEventListener(this.scheduledLogger, this.monitTraceDelay,
                                this.activitiEngine.getHistoryService(),
                        this.getLogger()),
                ActivitiEventType.PROCESS_COMPLETED);
        this.activitiEngine.getRuntimeService().addEventListener(
                        new ProcessCanceledEventListener(this.scheduledLogger, this.monitTraceDelay,
                                this.activitiEngine.getHistoryService(),
                        this.getLogger()),
                ActivitiEventType.PROCESS_CANCELLED);

        try {
            // Startup Activiti engine against running states of the SE:
            // - Activiti Engine must be started when the SE is in state 'STOPPED' to be able to deploy process
            // definitions
            // - In state 'STOPPED', the SE will not process incoming requests, so no process instance creation and no
            // user task completion will occur
            // - To avoid the executions of activities trigerred by timer or other events, the Activiti job executor
            // must be stopped when the SE is in state 'STOPPED'
            // TODO: Start the Activiti job executor

            // TODO: Add capability to disable the Activiti job executor:
            // For timers you can even dedicate one specific machine which only runs the job executor. (or more than one
            // to make them fault tolerant). Both the job executor and Activiti in general is designed in a way that
            // works clusterable out of the box. see : http://forums.activiti.org/content/clustering

            // TODO: Add JMX operation to start/stop the Activiti job executor when the component is started
            // TODO: Add JMX operation to disable/enable the Activiti job executor when the component is running
	        
		} catch( final ActivitiException e ) {
			throw new JBIException( "An error occurred while starting the Activiti BPMN Engine.", e );
        } finally {
            this.getLogger().fine("End ActivitiSE.doStart()");
        }
	}


    @Override
	public void doStop() throws JBIException {
        this.getLogger().fine("Start ActivitiSE.doStop()");

        try {
            this.scheduledLogger.shutdown();
            // TODO: The timeout should be configurable
            this.scheduledLogger.awaitTermination(5000, TimeUnit.MILLISECONDS);
        } catch (final InterruptedException e) {
            this.getLogger().log(Level.WARNING, "The termination of the scheduled logger was interrupted", e);
        }

		try {
            // TODO: Stop the Activiti Job Executor */

		} catch( final ActivitiException e ) {
			throw new JBIException( "An error occurred while stopping the Activiti BPMN Engine.", e );
        } finally {
            this.getLogger().fine("End ActivitiSE.doStop()");
		}
	}


    @Override
	public void doShutdown() throws JBIException {
        this.getLogger().fine("Start ActivitiSE.doStart()");

		try {
	        
			this.activitiEngine.close();

		} catch( final ActivitiException e ) {
			throw new JBIException( "An error occurred while shutdowning the Activiti BPMN Engine.", e );
        } finally {
            this.getLogger().fine("End ActivitiSE.doStart()");
		}
	}

    @Override
	protected AbstractServiceUnitManager createServiceUnitManager() {
		return new ActivitiSuManager(this);
	}

    private void registerCxfPetalsTransport() {
        final Bus bus = BusFactory.getThreadDefaultBus();
        final PetalsCxfTransportFactory cxfPetalsTransport = new PetalsCxfTransportFactory();
        final ConduitInitiatorManager extension = bus.getExtension(ConduitInitiatorManager.class);
        extension.registerConduitInitiator(PetalsCxfTransportFactory.TRANSPORT_ID, cxfPetalsTransport);
        // TODO: Set a timeout at CXF client level (it should be the same than the tiemout at NMR level)
        // TODO: Add unit tests about timeout
    }
}
