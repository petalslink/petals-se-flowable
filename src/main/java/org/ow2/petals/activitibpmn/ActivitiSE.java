/**
 * Copyright (c) 2014-2014 Linagora
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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.activiti.engine.ActivitiException;
import org.activiti.engine.ProcessEngine;
import org.activiti.engine.ProcessEngineConfiguration;

import javax.jbi.JBIException;

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
	 * @see java.util.concurrent.ConcurrentHashMap#put(java.lang.Object, java.lang.Object)
	 */
	public Map<EptAndOperation,ActivitiOperation> registerActivitiOperation(
			EptAndOperation eptAndOperation,
			ActivitiOperation activitiOperation ) {
		this.eptOperationToActivitiOperation.put(eptAndOperation, activitiOperation);
		return this.eptOperationToActivitiOperation;
	}


	/**
	 * @param eptAndOperation the end-point Name and operation Name
	 * @return the previous Activiti Operation that was registered for this end-point Name and operation Name
	 * @see java.util.concurrent.ConcurrentHashMap#remove(java.lang.Object)
	 */
	public ActivitiOperation removeActivitiOperation( EptAndOperation eptAndOperation) {
		return this.eptOperationToActivitiOperation.remove( eptAndOperation );
	}

	/**
	 * @param eptName the end-point Name
	 * @return the map without the deleted elements
	 * @see java.util.concurrent.ConcurrentHashMap#remove(java.lang.Object)
	 */
	public Map<EptAndOperation,ActivitiOperation> removeActivitiOperation( String eptName) {
		List<EptAndOperation> toRemove = new ArrayList<EptAndOperation>();
		for (EptAndOperation key: this.eptOperationToActivitiOperation.keySet()) {
		    if (key.getEptName() == eptName) {
		        toRemove.add(key);
		    }
		}
		for (EptAndOperation key: toRemove) {
			this.eptOperationToActivitiOperation.remove(key);
		}
		
		return this.eptOperationToActivitiOperation;
	}

	/**
	 * @param logLevel
	 */
	public void logEptOperationToActivitiOperation ( Logger logger, Level logLevel) {
		for (Map.Entry<EptAndOperation,ActivitiOperation> entry : eptOperationToActivitiOperation.entrySet())
		{
			logger.log(logLevel, "*** EptAndoperation ");
			logger.log(logLevel, "EptName = " + entry.getKey().getEptName());
			logger.log(logLevel, "OperationName = " + entry.getKey().getOperationName());
			logger.log(logLevel, "------------------------------------------------------ ");
			entry.getValue().logActivitiOperation(logger, logLevel);
			logger.log(logLevel, "******************* ");
		}
	}



	/**
	 * @param eptAndOperation the end-point Name and operation Name
	 * @return the Activiti Operation associated with this end-point name and operation Name
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public ActivitiOperation getActivitiOperations( EptAndOperation eptAndOperation) {
		return this.eptOperationToActivitiOperation.get(eptAndOperation);
	}
	
	
	/*
	 * (non-Javadoc)
	 * @see org.ow2.petals.component.framework.AbstractComponent
	 * #doInit()
	 */
	@Override
	public void doInit() throws JBIException {
		try {
		
	        this.getLogger().info("***********************");
			this.getLogger().info("*** Start doInit() in ActivitiSE");
	        
	        /* get Activiti database configuration */
			final String jdbcDriver = getComponentExtensions().get(ActivitiSEConstants.DBServer.JDBC_DRIVER);
			final String jdbcUrl = getComponentExtensions().get(ActivitiSEConstants.DBServer.JDBC_URL);
			final String jdbcUsername = getComponentExtensions().get(ActivitiSEConstants.DBServer.JDBC_USERNAME);
			final String jdbcPassword = getComponentExtensions().get(ActivitiSEConstants.DBServer.JDBC_PASSWORD);
			final String jdbcMaxActiveConnections = getComponentExtensions().get(ActivitiSEConstants.DBServer.JDBC_MAX_ACTIVE_CONNECTIONS);
			final String jdbcMaxIdleConnections = getComponentExtensions().get(ActivitiSEConstants.DBServer.JDBC_MAX_IDLE_CONNECTIONS);
			final String jdbcMaxCheckoutTime = getComponentExtensions().get(ActivitiSEConstants.DBServer.JDBC_MAX_CHECKOUT_TIME);
			final String jdbcMaxWaitTime = getComponentExtensions().get(ActivitiSEConstants.DBServer.JDBC_MAX_WAIT_TIME);
			 /* DATABASE_TYPE Possible values: {h2, mysql, oracle, postgres, mssql, db2}. */
			final String databaseType = getComponentExtensions().get(ActivitiSEConstants.DBServer.DATABASE_TYPE);
			/* DATABASE_SCHEMA_UPDATE Possible values: {false, true, create-drop } */
			final String databaseSchemaUpdate = getComponentExtensions().get(ActivitiSEConstants.DBServer.DATABASE_SCHEMA_UPDATE);

	        this.getLogger().info(" DB configuration - " + ActivitiSEConstants.DBServer.JDBC_DRIVER + " = " + jdbcDriver);
	        this.getLogger().info(" DB configuration - " + ActivitiSEConstants.DBServer.JDBC_URL + " = " + jdbcUrl);
	        this.getLogger().info(" DB configuration - " + ActivitiSEConstants.DBServer.JDBC_USERNAME + " = " + jdbcUsername);
	        this.getLogger().info(" DB configuration - " + ActivitiSEConstants.DBServer.JDBC_PASSWORD + " = " + jdbcPassword);
	        this.getLogger().info(" DB configuration - " + ActivitiSEConstants.DBServer.JDBC_MAX_ACTIVE_CONNECTIONS + " = " + jdbcMaxActiveConnections);
	        this.getLogger().info(" DB configuration - " + ActivitiSEConstants.DBServer.JDBC_MAX_IDLE_CONNECTIONS + " = " + jdbcMaxIdleConnections);
	        this.getLogger().info(" DB configuration - " + ActivitiSEConstants.DBServer.JDBC_MAX_CHECKOUT_TIME + " = " + jdbcMaxCheckoutTime);
	        this.getLogger().info(" DB configuration - " + ActivitiSEConstants.DBServer.JDBC_MAX_WAIT_TIME + " = " + jdbcMaxWaitTime);
	        this.getLogger().info(" DB configuration - " + ActivitiSEConstants.DBServer.DATABASE_TYPE + " = " + databaseType);
	        this.getLogger().info(" DB configuration - " + ActivitiSEConstants.DBServer.DATABASE_SCHEMA_UPDATE + " = " + databaseSchemaUpdate);
		    
	        /* TODO Test Activiti database connection configuration */
	        /* TODO Test the Database Schema Version
	         *      What about databaseSchemaUpdate values "true" and "create-drop" 
	         */
	        /* TODO Set the non set value with default value */

			/* Create an Activiti ProcessEngine with database configuration */
	        pec = ProcessEngineConfiguration.createStandaloneProcessEngineConfiguration();
			pec.setJdbcDriver(jdbcDriver);
            pec.setJdbcUrl(jdbcUrl);           
            pec.setJdbcUsername(jdbcUsername).setJdbcPassword(jdbcPassword);
            pec.setDatabaseSchemaUpdate(databaseSchemaUpdate);
            pec.setJobExecutorActivate(false);

            this.activitiEngine = pec.buildProcessEngine();
			
			this.getLogger().info("*** End doInit() in ActivitiSE");
	        this.getLogger().info("***********************");

		} catch( final ActivitiException e ) {
			throw new JBIException( "An error occurred while creating the Activiti BPMN Engine.", e );
		}
	}


	/*
	 * (non-Javadoc)
	 * @see org.ow2.petals.component.framework.AbstractComponent
	 * #doStart()
	 */
	@Override
	public void doStart() throws JBIException {
		try {

	        this.getLogger().info("***********************");
			this.getLogger().info("*** Start doStart() in ActivitiSE");
	        
	        /* TODO Start Job Executor */
			/*   For timers you can even dedicate one specifc machine which only runs the job executor.
			 *   (or more than one to make them fault tolerant).
			 *   Both the job executor and Activiti in general is designed in a way that works clusterable out of the box.
			 *   see : http://forums.activiti.org/content/clustering
			 */

	        /* TODO Build Process Engine */
	        
			this.getLogger().info("*** End doStart() in ActivitiSE");
	        this.getLogger().info("***********************");
	        
		} catch( final ActivitiException e ) {
			throw new JBIException( "An error occurred while starting the Activiti BPMN Engine.", e );
		} 
	}


	/*
	 * (non-Javadoc)
	 * @see org.ow2.petals.component.framework.AbstractComponent
	 * #doStop()
	 */
	@Override
	public void doStop() throws JBIException {
		try {
	        this.getLogger().info("***********************");
			this.getLogger().info("*** Start doStop() in ActivitiSE");
	        
	        /* TODO STOP Job Executor */
			/* TODO this.activitiEngine.standby(); */
	        
			this.getLogger().info("*** End doStop() in ActivitiSE");
	        this.getLogger().info("***********************");

		} catch( final ActivitiException e ) {
			throw new JBIException( "An error occurred while stopping the Activiti BPMN Engine.", e );
		}
	}


	/*
	 * (non-Javadoc)
	 * @see org.ow2.petals.component.framework.AbstractComponent
	 * #doShutdown()
	 */
	@Override
	public void doShutdown() throws JBIException {
		try {
	        this.getLogger().info("***********************");
			this.getLogger().info("*** Start doStart() in ActivitiSE");
	        
			this.activitiEngine.close();
	        
			this.getLogger().info("*** End doStart() in ActivitiSE");
	        this.getLogger().info("***********************");

		} catch( final ActivitiException e ) {
			throw new JBIException( "An error occurred while shutdowning the Activiti BPMN Engine.", e );
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.ow2.petals.component.framework.se.AbstractServiceEngine
	 * #createServiceUnitManager()
	 */
	@Override
	protected AbstractServiceUnitManager createServiceUnitManager() {
		return new ActivitiSuManager(this);
	}

}

