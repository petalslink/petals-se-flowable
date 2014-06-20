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


import java.io.FileInputStream;
import java.io.FileNotFoundException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.activiti.engine.ActivitiException;
import org.activiti.engine.ProcessEngine;
import org.activiti.engine.ProcessEngineConfiguration;
import org.activiti.engine.RepositoryService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.TaskService;

import org.activiti.engine.repository.DeploymentBuilder;
import org.activiti.engine.runtime.ProcessInstance;
import org.activiti.engine.task.Task;
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
	private RepositoryService repS;
	private RuntimeService runS;
	
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
			/* Create a stand-alone in Activiti Engine with h2 server base on localhost */
			
			
			
			this.getLogger().info("*** Start Init Activiti BPMN Service Engine...");
	        this.getLogger().info("***********************");
			
			pec = ProcessEngineConfiguration.createStandaloneProcessEngineConfiguration();
			pec.setJdbcDriver("org.h2.Driver");
            pec.setJdbcUrl("jdbc:h2:tcp://localhost//Users/bescudie/my-own-db;DB_CLOSE_DELAY=1000");           
            pec.setJdbcUsername("sa").setJdbcPassword("");
            pec.setDatabaseSchemaUpdate(ProcessEngineConfiguration.DB_SCHEMA_UPDATE_TRUE);
            pec.setJobExecutorActivate(false);

            this.activitiEngine = pec.buildProcessEngine();
			
            this.getLogger().info("***********************");
			this.getLogger().info("*** End Init Activiti BPMN Service Engine...");
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
			this.getLogger().info("*** Start doStart Activiti BPMN Service Engine...");
	        this.getLogger().info("***********************");			
			
            /* deploy Demo processes */
			repS = this.activitiEngine.getRepositoryService();
			runS = this.activitiEngine.getRuntimeService();
			
			String bpmnFileName;
			FileInputStream bpmnInputFile;
			DeploymentBuilder db;
			ProcessInstance pI;

			
			this.getLogger().info("***********************");
			this.getLogger().info("*** Test Activiti SE  process deployment");
	        this.getLogger().info("***********************");						
			
			/* print list of existing process */
			/*
			String deploymentName = "Demo processes";
		    List<Deployment> deploymentList = repS.createDeploymentQuery().deploymentName(deploymentName).list();
		    
		    if (deploymentList == null || deploymentList.size() == 0) {
		    	db = repS.createDeployment();
		        db.name(deploymentName);
		        db.addClasspathResource("org/activiti/explorer/demo/process/createTimersProcess.bpmn20.xml");
		        db.addClasspathResource("org/activiti/explorer/demo/process/VacationRequest.bpmn20.xml");
		        db.addClasspathResource("org/activiti/explorer/demo/process/VacationRequest.png");
		        db.addClasspathResource("org/activiti/explorer/demo/process/FixSystemFailureProcess.bpmn20.xml");
		        db.addClasspathResource("org/activiti/explorer/demo/process/FixSystemFailureProcess.png");
		        db.addClasspathResource("org/activiti/explorer/demo/process/simple-approval.bpmn20.xml");
		        db.addClasspathResource("org/activiti/explorer/demo/process/Helpdesk.bpmn20.xml");
		        db.addClasspathResource("org/activiti/explorer/demo/process/Helpdesk.png");
		        db.addClasspathResource("org/activiti/explorer/demo/process/reviewSalesLead.bpmn20.xml");
		        db.enableDuplicateFiltering();
		        db.deploy();
		    	}
		     */
			
			/*
			 * bpmnFileName = "/Users/bescudie/Dev Java/src/Activiti/modules/activiti-engine/src/test/resources/org/activiti/examples/bpmn/callactivity/orderProcess.bpmn20.xml";
			bpmnFile = new FileInputStream(bpmnFileName);		  
			 */
			db = repS.createDeployment();
			/* Put the process name (process id from BPMN20.xml file) in order to allow DuplicateFiltering */
			/* put NAME_ in ACT_GE_DEPLOYMENT table */
			String depName = "Test Activiti SE";
			db.name(depName);
			/* retrieve id of the process in the xml file */
			/* put NAME_ in ACT_GE_BYTEARRAY */
			bpmnFileName = "/Users/bescudie/Test-Petals-StandAlone/Process/orderProcess.bpmn20.xml";
			bpmnInputFile = new FileInputStream(bpmnFileName);		  
			db.addInputStream("orderProcess.bpmn20.xml", bpmnInputFile);
			/*
			 * File bpmnFile = new File(bpmnFileName);
			 * URL bpmnUrl = bpmnFile.toURI().toURL();
			 */
			
			db.addClasspathResource("process/VacationRequest.bpmn20.xml");
			
			/* 	db.activateProcessDefinitionsOn(activationDate);  seem to don't work*/
			
			db.enableDuplicateFiltering();
			db.deploy();
			
			this.getLogger().info("***********************");
			this.getLogger().info("*** Test Activiti SE  deploy()");
	        this.getLogger().info("***********************");						
			this.getLogger().info("*** Test Activiti SE  activate process");
	        this.getLogger().info("***********************");						

			
			Map<String, Object> variables = new HashMap<String, Object>();
			variables.put("employeeName", "Kermit");
			variables.put("numberOfDays", new Integer(4));
			variables.put("vacationMotivation", "I'm really tired!");
			      
			pI = runS.startProcessInstanceByKey("vacationRequest", variables);
			      
			// Verify that we started a new process instance
			this.getLogger().info("***********************************");
			this.getLogger().info("****** Number of process instances: " + runS.createProcessInstanceQuery().count());           
			this.getLogger().info("***********************************");
			
			// Fetch all tasks for the management group
			TaskService taskService = this.activitiEngine.getTaskService();
			List<Task> tasks = taskService.createTaskQuery().taskCandidateGroup("management").list();
			this.getLogger().info("***********************************");
			for (Task task : tasks) {
				this.getLogger().info("***** Task available: " + task.getName());
			}  
			this.getLogger().info("***********************************");
			
			Task task = tasks.get(0);
		      
			Map<String, Object> taskVariables = new HashMap<String, Object>();
			taskVariables.put("vacationApproved", "false");
			taskVariables.put("managerMotivation", "We have a tight deadline!");
			taskService.complete(task.getId(), taskVariables); 
			
			

		} catch( final ActivitiException e ) {
			throw new JBIException( "An error occurred while starting the Activiti BPMN Engine.", e );
		} catch (final FileNotFoundException e) {
			throw new JBIException( "An error occurred while starting the Activiti BPMN unable to find file.", e );
		} /* catch (IOException e) {
			throw new JBIException( "An error occurred while creating file logBES.log.", e );
		}   catch (final MalformedURLException e) {
			throw new JBIException( "An error occurred while starting the Activiti BPMN unable to form URL", e );
		} */
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

