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

import java.io.File;
import java.net.MalformedURLException;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.JBIException;

import org.activiti.engine.ActivitiException;
import org.activiti.engine.ProcessEngine;
import org.activiti.engine.ProcessEngineConfiguration;
import org.ow2.petals.activitibpmn.operation.ActivitiOperation;
import org.ow2.petals.component.framework.se.AbstractServiceEngine;
import org.ow2.petals.component.framework.su.AbstractServiceUnitManager;


/**
 * The component class of the Activiti BPMN Service Engine.
 * @author Bertrand Escudie - Linagora
 */
public class ActivitiSE extends AbstractServiceEngine {
	
	/**
	 * The Activiti BPMN Engine.
	 */
	private ProcessEngine activitiEngine;
	private ProcessEngineConfiguration pec;
	
	/**
	 * @return the Activiti Engine
	 */
	public ProcessEngine getProcessEngine() {
		return this.activitiEngine;
	}
	
	/**
     * A map used to get the Activiti Operation associated with (end-point Name + Operation)
     */
    private final Map<EptAndOperation,ActivitiOperation> eptOperationToActivitiOperation =
        new ConcurrentHashMap<EptAndOperation, ActivitiOperation > ();
	
	
  	/**
	 * @param eptAndOperation the end-point Name and operation Name
	 * @param activitiOperation the Activiti Operation
	 * @return the map with the inserted elements
	 */
    public void registerActivitiOperation(final EptAndOperation eptAndOperation,
            final ActivitiOperation activitiOperation) {
        this.eptOperationToActivitiOperation.put(eptAndOperation, activitiOperation);
    }

	    /**
     * @param eptName
     *            the end-point name
     */
    public void removeActivitiOperation(final String eptName) {

        final Iterator<Entry<EptAndOperation, ActivitiOperation>> itEptOperationToActivitiOperation = this.eptOperationToActivitiOperation
                .entrySet().iterator();
        while (itEptOperationToActivitiOperation.hasNext()) {
            final Entry<EptAndOperation, ActivitiOperation> entry = itEptOperationToActivitiOperation.next();
            if (entry.getKey().getEptName().equals(eptName)) {
                itEptOperationToActivitiOperation.remove();
            }
        }
	}

	/**
	 * @param logLevel
	 */
    public void logEptOperationToActivitiOperation(final Logger logger, final Level logLevel) {
        if (logger.isLoggable(logLevel)) {
            for (final Map.Entry<EptAndOperation, ActivitiOperation> entry : eptOperationToActivitiOperation.entrySet()) {
                final EptAndOperation key = entry.getKey();
                logger.log(logLevel, "*** EptAndoperation ");
                logger.log(logLevel, "EptName = " + key.getEptName());
                logger.log(logLevel, "OperationName = " + key.getOperationName());
                logger.log(logLevel, "------------------------------------------------------ ");
                entry.getValue().log(logger, logLevel);
                logger.log(logLevel, "******************* ");
            }
        }
	}



	/**
	 * @param eptAndOperation the end-point Name and operation Name
	 * @return the Activiti Operation associated with this end-point name and operation Name
	 */
    public ActivitiOperation getActivitiOperations(final EptAndOperation eptAndOperation) {
		return this.eptOperationToActivitiOperation.get(eptAndOperation);
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
		    
	        /* TODO Test Activiti database connection configuration */
	        /* TODO Test the Database Schema Version
	         *      What about databaseSchemaUpdate values "true" and "create-drop" 
	         */
	        /* TODO Set the non set value with default value */

			/* Create an Activiti ProcessEngine with database configuration */
            this.pec = ProcessEngineConfiguration.createStandaloneProcessEngineConfiguration();
            this.pec.setJdbcDriver(jdbcDriver);
            this.pec.setJdbcUrl(jdbcUrl);
            this.pec.setJdbcUsername(jdbcUsername).setJdbcPassword(jdbcPassword);
            this.pec.setJdbcMaxActiveConnections(jdbcMaxActiveConnections);
            this.pec.setJdbcMaxIdleConnections(jdbcMaxIdleConnections);
            this.pec.setJdbcMaxCheckoutTime(jdbcMaxCheckoutTime);
            this.pec.setJdbcMaxWaitTime(jdbcMaxWaitTime);
            this.pec.setDatabaseSchemaUpdate(databaseSchemaUpdate);
            this.pec.setJobExecutorActivate(false);

            this.activitiEngine = pec.buildProcessEngine();

		} catch( final ActivitiException e ) {
			throw new JBIException( "An error occurred while creating the Activiti BPMN Engine.", e );
        } finally {
            this.getLogger().fine("End ActivitiSE.doInit()");
		}
	}


    @Override
	public void doStart() throws JBIException {
        this.getLogger().fine("Start ActivitiSE.doStart()");

        try {
	        /* TODO Start Job Executor */
			/*   For timers you can even dedicate one specifc machine which only runs the job executor.
			 *   (or more than one to make them fault tolerant).
			 *   Both the job executor and Activiti in general is designed in a way that works clusterable out of the box.
			 *   see : http://forums.activiti.org/content/clustering
			 */

	        /* TODO Build Process Engine */
	        
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
	        /* TODO STOP Job Executor */
			/* TODO this.activitiEngine.standby(); */

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
}