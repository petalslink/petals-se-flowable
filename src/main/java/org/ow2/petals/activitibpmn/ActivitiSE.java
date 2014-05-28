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

import javax.jbi.JBIException;

import org.ow2.petals.component.framework.se.AbstractServiceEngine;
import org.ow2.petals.component.framework.su.AbstractServiceUnitManager;
import org.activiti.engine.ProcessEngine;
import org.activiti.engine.ProcessEngineConfiguration;
/* import org.activiti.engine.ProcessEngines; */
import org.activiti.engine.ActivitiException;


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
	 * @return the Activiti Engine
	 */
	public ProcessEngine getProcessEngine() {
		return this.activitiEngine;
	}

	/*
	 * (non-Javadoc)
	 * @see org.ow2.petals.component.framework.AbstractComponent
	 * #doInit()
	 */
	@Override
	public void doInit() throws JBIException {
		try {
			/* this.activitiEngine = ProcessEngines.getDefaultProcessEngine(); */
			
			/* Create a standalone in memory default Activiti Engine */
			this.activitiEngine = ProcessEngineConfiguration.createStandaloneInMemProcessEngineConfiguration()
					  .setDatabaseSchemaUpdate(ProcessEngineConfiguration.DB_SCHEMA_UPDATE_FALSE)
					  .setJdbcUrl("jdbc:h2:mem:my-own-db;DB_CLOSE_DELAY=1000")
					  .setJobExecutorActivate(false)
					  .buildProcessEngine();
			
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
			/* this.activitiEngine.start(); */

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
			/* this.activitiEngine.standby(); */

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
			this.activitiEngine.close();

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

