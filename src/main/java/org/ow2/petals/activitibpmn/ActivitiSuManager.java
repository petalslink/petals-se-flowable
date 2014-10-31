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

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.StringWriter;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import javax.jbi.servicedesc.ServiceEndpoint;
import javax.xml.namespace.QName;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.activiti.engine.RepositoryService;
import org.activiti.engine.impl.util.io.InputStreamSource;
import org.activiti.engine.repository.Deployment;
import org.activiti.engine.repository.DeploymentBuilder;
import org.activiti.engine.repository.Model;
import org.activiti.engine.repository.ProcessDefinition;
import org.activiti.bpmn.converter.*;
import org.activiti.bpmn.model.BpmnModel;
import org.activiti.bpmn.model.FormValue;
import org.activiti.bpmn.model.StartEvent;
import org.activiti.bpmn.model.UserTask;
import org.ow2.petals.activitibpmn.ActivitiSEConstants.BpmnActionType;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.AbstractComponent;
import org.ow2.petals.component.framework.su.ServiceUnitDataHandler;
import org.ow2.petals.component.framework.api.configuration.ConfigurationExtensions;
import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;
import org.ow2.petals.component.framework.jbidescriptor.generated.Jbi;
import org.ow2.petals.component.framework.jbidescriptor.generated.Provides;
import org.ow2.petals.component.framework.su.AbstractServiceUnitManager;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.ebmwebsourcing.easycommons.lang.StringHelper;


/**
 * @author Bertrand ESCUDIE - Linagora
 */
public class ActivitiSuManager extends AbstractServiceUnitManager {

	private RepositoryService repS;	
	
	private class ProcessBPMN {
		public String processFileName;
		public String version;
		
		public ProcessBPMN (String processFileName , String version){		
			this.processFileName = processFileName;
			this.version = version;
		}
	}	

	/**
	 * Default constructor.
	 * @param component the ACTIVITI component
	 */
	public ActivitiSuManager(AbstractComponent component) {
		super(component);
		if( logger.isLoggable( Level.INFO )) {
			logger.info("****************************");
	    	logger.info("*** Start ActivitiSUManager() in ActivitiSuManager");
		}
		if( logger.isLoggable( Level.INFO )) {
	    	logger.info("*** End ActivitiSUManager() in ActivitiSuManager");
	    	logger.info("****************************");
		}
	}
	
    /*
     * (non-Javadoc)
     * @see org.ow2.petals.component.framework.su.AbstractServiceUnitManager
     * #doDeploy(java.lang.String, java.lang.String, org.ow2.petals.component.framework.jbidescriptor.generated.Jbi)
     */
    @Override
    protected void doDeploy( String serviceUnitName, String suRootPath, Jbi jbiDescriptor )
    throws PEtALSCDKException {
		if( logger.isLoggable( Level.INFO )) {
			logger.info("****************************");
			logger.info("*** Start doDeploy() in ActivitiSuManager");
			logger.info("*** SU =" + serviceUnitName );
		}
        // Get the Activiti RepositoryService
	    if( repS == null) repS = ((ActivitiSE) component).getProcessEngine().getRepositoryService();
		
		// Check the JBI descriptor
		if( jbiDescriptor == null || jbiDescriptor.getServices() == null
				|| jbiDescriptor.getServices().getProvides() == null || jbiDescriptor.getServices().getProvides().size() == 0 )
			throw new PEtALSCDKException( "Invalid JBI descriptor: it does not contain a 'provides' section." );

		//TODO Manage consume for Activiti Petals Task
        if( jbiDescriptor.getServices().getConsumes().size() != 0 )
            throw new PEtALSCDKException( "Invalid JBI descriptor: 'Consumes' sections are not till supported by this component." );

		// Check that there is only one Provides section in the SU
		if( jbiDescriptor.getServices().getProvides().size() != 1 )
			throw new PEtALSCDKException( "Invalid JBI descriptor: it must not have more than one 'provides' section." );

        // Get the provides
        Provides provides = jbiDescriptor.getServices().getProvides().get( 0 );
		if( provides == null )
			throw new PEtALSCDKException( "Invalid JBI descriptor: the 'provides' section is invalid." );
	    // Deploy the processes from the file bpmn20.xml described in jbi.xml of the SU
		Map<String, ProcessDefinition> processDefinitions = deployActivitiProcess ( provides, suRootPath );

		// get the serviceEndPoint Name  of the Provides
		String edptName = provides.getEndpointName();
	    // Get the endpoint of the provides
		ServiceEndpoint  serviceEndpoint = this.getEndpointsForServiceUnit(serviceUnitName).iterator().next();
        // get the Wsdl
        Document wsdlDocument = this.getServiceDescription(serviceEndpoint) ;
        // Get the EptOperation and ActivitiOperation mapping and store in the eptOperationToActivitiOperation Map  
        getActivitiOperationMapping(wsdlDocument, edptName, processDefinitions);
        
		if( logger.isLoggable( Level.INFO )) {
			logger.info("*** End doDeploy() in ActivitiSuManager");
			logger.info("***********************");		
		}
    }

    /*
     * (non-Javadoc)
     * @see org.ow2.petals.component.framework.su.AbstractServiceUnitManager
     * #doStart(java.lang.String)
     */
    @Override
    protected void doStart( String serviceUnitName ) throws PEtALSCDKException {
    	logger.info("****************************");
    	logger.info("*** Start doStart() in ActivitiSuManager");
    	logger.info("*** SU =" + serviceUnitName );

		// TODO Manage the process suspension State be careful of multi-SU deployment for the same process
    	
    	logger.info("*** End doStart() in ActivitiSuManager");
		logger.info("***********************");		
    }


    /*
     * (non-Javadoc)
     * @see org.ow2.petals.component.framework.su.AbstractServiceUnitManager
     * #doStop(java.lang.String)
     */
    @Override
    protected void doStop( String serviceUnitName ) throws PEtALSCDKException {
    	logger.info("****************************");
    	logger.info("*** Start doStop() in ActivitiSuManager");
    	logger.info("*** SU =" + serviceUnitName );
		// TODO Manage the process suspension State: be careful of multi SU deployement for the same process
    	logger.info("*** End doStop() in ActivitiSuManager");
		logger.info("***********************");		
    }


    /*
     * (non-Javadoc)
     * @see org.ow2.petals.component.framework.su.AbstractServiceUnitManager
     * #doUndeploy(java.lang.String)
     */
    @Override
    protected void doUndeploy( String serviceUnitName ) throws PEtALSCDKException {
    	logger.info("****************************");
    	logger.info("*** Start doUndeploy() in ActivitiSuManager");
    	logger.info("*** SU =" + serviceUnitName );
        EptAndOperation eptAndOperation;

        // Get the service end point associated to the service Unit Name
 		ServiceEndpoint serviceEndpoint = this.getEndpointsForServiceUnit(serviceUnitName).iterator().next();
        // set the serviceEndPoint Name  of the Provides
 		String edptName = serviceEndpoint.getEndpointName();
		// Remove the ActivitiOperation in the map with the corresponding end-point
		((ActivitiSE) this.component).removeActivitiOperation(edptName);  	

 		
 /**        
 		// Get the operation Name of the end point
 		ServiceUnitDataHandler suDataHandler =this.getSUDataHandlerForEndpoint(serviceEndpoint);
 		List<QName> operations = suDataHandler.getEndpointOperationsList(serviceEndpoint);
 		
 		for ( QName operation : operations) {
 			eptAndOperation = new EptAndOperation(edptName,operation.getLocalPart());
 			// Remove the ActivitiOperation in the map with the corresponding end-point and Operation
 			((ActivitiSE) this.component).removeActivitiOperation(eptAndOperation);  	
 			logger.info("*** ept: "+ eptAndOperation.getEptName() + " operation : "+ eptAndOperation.getOperationName());
 			logger.info("          is removed from MAP eptOperationToActivitiOperation" );
 		}
*/   		
		// TODO Manage the undeployement of the process: be careful of multi SU deployement for the same process
    	logger.info("*** End doUndeploy() in ActivitiSuManager");
		logger.info("***********************");		
   }	

    
	/**
	 * deploy in Activiti DB the process if it doesn't exist
	 *    Process deployment is characterized in Activiti Database by processName / tenantId / categoryId / version
     *    tenantId allows to have several instance of the same process model in different contexts: different owner, assignee, group ....
     *    categoryId allows to manage process lifeCycle: Dev, PreProd, Prod ...
     *	  version allows to manage several versions 
     *            The first time a process with a particular key is deployed, version 1 is assigned.
     *            For all subsequent deployments of process definitions with the same key, the version will be set 1
     *            higher then the maximum currently deployed version.
 	 * 
	 * @param provides - the provides that the SU expose
	 * @param suRootPath path of the process file bpmn20.xml
	 * @return a Map<String processKey,ProcessDefinition> of the corresponding processes described in the SU
	 * @throws PEtALSCDKException 
	 */
    private Map< String, ProcessDefinition> deployActivitiProcess (
    			Provides provides,
    			String suRootPath
    			) throws PEtALSCDKException{
	
	    Map<String, ProcessDefinition> processDefinitions = new ConcurrentHashMap<String, ProcessDefinition>();
		ProcessDefinition processDefinition;

	    // Get the SU Data handler
	    ServiceUnitDataHandler suDataHandler = getSUDataHandlerForProvides( provides );
	    if( suDataHandler == null )
			throw new PEtALSCDKException( "Error while processing the JBI descriptor in the component. The SU data handler was null." );
        // Get the extension configuration for the Activiti process(es) to be deployed from the SU jbi.xml
		ConfigurationExtensions extensions = suDataHandler.getConfigurationExtensions( provides );
		if( extensions == null )
			throw new PEtALSCDKException( "Invalid JBI descriptor: it does not contain any component extension." );
		String tenantId = extensions.get(ActivitiSEConstants.TENANT_ID);
		if (tenantId == null) tenantId = "myTenant"; // default value 
		String categoryId = extensions.get(ActivitiSEConstants.CATEGORY_ID);
		if (categoryId == null) tenantId = "myCategory"; // default value 
		final Set<ProcessBPMN> processes = new HashSet<ProcessBPMN>();
		String processFileName = extensions.get(ActivitiSEConstants.PROCESS_FILE);
		String version = extensions.get(ActivitiSEConstants.VERSION);
		Integer nbProcesses = 1;
		if ( processFileName == null || version == null ) {
			// Several process description
			processFileName = extensions.get(ActivitiSEConstants.PROCESS_FILE + nbProcesses.toString());
			version = extensions.get(ActivitiSEConstants.VERSION + nbProcesses.toString());
			while ( !( processFileName == null || version == null ) ) {
				processes.add(new ProcessBPMN(processFileName,version));
				nbProcesses++;
				processFileName = extensions.get(ActivitiSEConstants.PROCESS_FILE + nbProcesses.toString());
				version = extensions.get(ActivitiSEConstants.VERSION + nbProcesses.toString());
 			}
			if ( nbProcesses == 1)
				throw new PEtALSCDKException( "Invalid JBI descriptor: it does not contain any process description." );
		}
		else {
			// One process description
			processes.add(new ProcessBPMN(processFileName,version));
		}
        // Deploy the processes from the file bpmn20.xml
		if( logger.isLoggable( Level.INFO )) {
			logger.info("*** processes configuration");
			logger.info("*** tenantId =" + tenantId);
			logger.info("*** categoryId = " + categoryId);
			logger.info("*** nb processBPMN = " + processes.size());
		}
		
		ProcessBPMN process;
		List<ProcessDefinition> processDefinitionSearchList;
		
		// deploy the processes
		Iterator<ProcessBPMN> iterator = processes.iterator();
		while (iterator.hasNext() ){
			process = iterator.next();
		    // Deploy the process from the file bpmn20.xml
			
		    // Check that the process is not already deployed
			// If it exist do not deploy it again then return it ProcessDefinition
			// This allow to deploy the same SU (porcess.bpmn20.xml) on several Service Engine petals-se-activitibpmn
			// to create several Petals service endpoint for the same process/tenantId/categoryId/version.
		    processDefinitionSearchList = repS.createProcessDefinitionQuery().processDefinitionResourceName(process.processFileName)
		    		    .processDefinitionCategory(categoryId).processDefinitionTenantId(tenantId)
		    		    .processDefinitionVersion(Integer.valueOf(process.version)).list();
	 		    
		    if (processDefinitionSearchList == null || processDefinitionSearchList.size() == 0) {
				final DeploymentBuilder db = repS.createDeployment();

				// Characterize the deployement with processFileName / tenantId / CategoryId
				db.name(process.processFileName);
				db.tenantId(tenantId);
				db.category(categoryId);

				// add the process definition from file: file Name, file Path 
		    	String processFilePath = suRootPath + process.processFileName;
		    	if( processFilePath != null )
					processFilePath = processFilePath.trim();
				if( StringHelper.isNullOrEmpty( processFilePath ))
					throw new PEtALSCDKException( "The '" + ActivitiSEConstants.PROCESS_FILE + "' parameter is not well defined or is empty." );
				FileInputStream bpmnInputFile;
				try {
					bpmnInputFile = new FileInputStream(processFilePath);
				} catch (final FileNotFoundException e) {
					throw new PEtALSCDKException( "An error occurred while starting the Activiti BPMN unable to find file: " + processFilePath, e );
				}
				// add BPMN Model in order to change the category in the Process definition
				// that is indeed derived from the targetNameSpace of the bpmn20.xml file
				org.activiti.bpmn.model.BpmnModel bpmnModel;
				InputStreamSource bpmnInputStreamSource = new InputStreamSource( bpmnInputFile );
				
				bpmnModel = new BpmnXMLConverter().convertToBpmnModel(bpmnInputStreamSource, false, false);
				bpmnModel.setTargetNamespace(categoryId);

				db.addBpmnModel(processFileName, bpmnModel);
			
				// TODO manage the assignee according with su jbi descriptor
				
				// TODO Manage the process suspension State be careful of multi SU deployement for the same process
				
				// Do not use db.enableDuplicateFiltering(); with management of tenantId and CategoryId
				Deployment deployment = db.deploy();

				// Set processDefinition
				processDefinition = repS.createProcessDefinitionQuery().deploymentId(deployment.getId()).list().get(0);
				processDefinitions.put(processDefinition.getKey(), processDefinition);
						
			    // TODO TO TEST Set Version of processDefinition
				//TEST avec createNativeDeploymentQuery  sur Deployment ce qui suit change la version du modele non du peloiement
/**				Model model = repS.createModelQuery().deploymentId(deployment.getId())
						.latestVersion().modelTenantId(tenantId).modelCategory(categoryId).list().get(0);
				model.setVersion( Integer.valueOf(process.version));
*/										
				if( logger.isLoggable( Level.INFO ))
					logger.info("*** process " + process.processFileName + " version: " + process.version +" is succesfully deployed");
			}
			else {
				if( logger.isLoggable( Level.INFO ))
					logger.info("*** process: " + process.processFileName + " version: " + process.version + " is already deployed");
				// Set processDefinition
				processDefinition= processDefinitionSearchList.get(0);
				processDefinitions.put(processDefinition.getKey(), processDefinition);
			}
		}

		// Log the processDefinitionList
		if( logger.isLoggable( Level.FINEST )) {
			for (Map.Entry<String, ProcessDefinition> entry : processDefinitions.entrySet()) {	
				processDefinition = entry.getValue();
				logger.finest("*** Process definition Id            = " + processDefinition.getId() );
				logger.finest("*** Process definition Category      = " + processDefinition.getCategory() );
				logger.finest("*** Process definition Name          = " + processDefinition.getName() );
				logger.finest("*** Process definition Key           = " + processDefinition.getKey() );
				logger.finest("*** Process definition Version       = " + processDefinition.getVersion() );
				logger.finest("*** Process definition Deployemnt Id = " + processDefinition.getDeploymentId() );
				logger.finest("*** Process definition ResourceName  = " + processDefinition.getResourceName() );
				logger.finest("*** Process definition TenantId      = " + processDefinition.getTenantId() );
			}
		}
		
	    return processDefinitions;
    }
    
    
    
	/**
	 * Read the Wsdl associated with the provides in order to get the EptOperation and ActivitiOperation mapping
	 * Store the mapping in the eptOperationToActivitiOperation Maps
 	 * 
	 * @param wsdlDocument - the Wsdl corresponding to the provides
	 * @param edptName - the endpoint Name
	 * @param processDefinitions - the Map of Activiti processKey and processDefintion
	 * @return void
	 * @throws PEtALSCDKException 
	 */
    
    // Get the EptOperation and ActivitiOperation mapping and store in the eptOperationToActivitiOperation Map  

    private void getActivitiOperationMapping (
    		Document wsdlDocument,
    		String edptName,
    		Map<String, ProcessDefinition> processDefinitions
    		) throws PEtALSCDKException {
    	try {
    		DOMSource domSource = new DOMSource(wsdlDocument);
    		StringWriter writer = new StringWriter();
    		StreamResult result = new StreamResult(writer);
    		TransformerFactory tf = TransformerFactory.newInstance();
    		Transformer transformer = tf.newTransformer();
    		transformer.transform(domSource, result);
    		wsdlDocument.getDocumentElement().normalize();
    		// Get the node "wsdl:binding"
    		Node binding = wsdlDocument.getElementsByTagNameNS("http://schemas.xmlsoap.org/wsdl/", "binding").item(0);
    		// Get the list of nodes "wsdl:operation"
    		NodeList operations = ((Element) binding).getElementsByTagNameNS("http://schemas.xmlsoap.org/wsdl/", "operation");
    	  
    		EptAndOperation eptAndOperation;
    		ActivitiOperation activitiOperation;
    		String bpmnProcessKey;
    		String processDefinitionId;
    		String bpmnAction;
    		BpmnActionType bpmnActionType;
    		Properties bpmnProcessId;
    		Properties bpmnUserId;
    		Properties bpmnVarInMsg;
    		Properties outMsgBpmnVar;
    		Properties faultMsgBpmnVar;
    		Map<String, org.activiti.bpmn.model.FormProperty > bpmnVarType;
    		Set<String> bpmnVarList;
    	  
    		for (int j=0; j < operations.getLength(); j++ ) {
    			Node operation = operations.item(j);
    			// set the eptAndOperation
    			eptAndOperation = new EptAndOperation( edptName,((Element) operation).getAttribute("name") );
    			// Get the node "bpmn:operation"
    			Node bpmnOperation = ((Element) operation).getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0", "operation").item(0);
    			// set the bpmnProcessKey
    			bpmnProcessKey = ((Element) bpmnOperation).getAttribute("bpmnProcess");
    			// Test the bpmnProcess value with processKey of the Map processDefinitions and set processDefinitionId
    			if ( processDefinitions.containsKey(bpmnProcessKey) ) {
    				processDefinitionId = processDefinitions.get(bpmnProcessKey).getId();
    			}
    			else throw new PEtALSCDKException("Malformed Wsdl: business process Key = " + bpmnProcessKey 
    					+ " does not correspond to any Business Process file of the SU " );
    			// set the bpmnAction
    			bpmnAction = ((Element) bpmnOperation).getAttribute("bpmnAction");
    			// set the bpmnActionType
    			if ( ((Element) bpmnOperation).getAttribute("bpmnActionType").equalsIgnoreCase("startEvent") ) bpmnActionType = BpmnActionType.START_EVENT;
    			else if ( ((Element) bpmnOperation).getAttribute("bpmnActionType").equalsIgnoreCase("userTask") ) bpmnActionType = BpmnActionType.USER_TASK;
    			else throw new PEtALSCDKException("Malformed Wsdl: Unauthorized BpmnActionType :" 
    					+ ((Element) bpmnOperation).getAttribute("bpmnActionType") + " in wsdl operation = "
    					+ ((Element) operation).getAttribute("name"));
    			// Get the node "bpmn:processId" and test it has always an InMsg attribute for bpmnActionType == BpmnActionType.USER_TASK
    			Node processId = ((Element) operation).getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0", "processId").item(0);
    			bpmnProcessId = new Properties();
    			if (processId != null) {
    				// set the processId properties
    				if ( ((Element) processId).getAttribute("inMsg") != null && ((Element) processId).getAttribute("inMsg") != "" )
    					bpmnProcessId.put("inMsg",((Element) processId).getAttribute("inMsg"));
    				else {
    					if ( bpmnActionType == BpmnActionType.USER_TASK )
    						throw new PEtALSCDKException("Malformed Wsdl: bpmn:processId attribute inMsg is mandatory for "
    								+ "bpmnActionType =\"userTask\" in wsdl operation = " + ((Element) operation).getAttribute("name"));
    				}
    				if ( ((Element) processId).getAttribute("outMsg") != null && ((Element) processId).getAttribute("outMsg") != "" )
    					bpmnProcessId.put("outMsg",((Element) processId).getAttribute("outMsg"));
    				if ( ((Element) processId).getAttribute("faultMsg") != null && ((Element) processId).getAttribute("faultMsg") != "" )
    					bpmnProcessId.put("faultMsg",((Element) processId).getAttribute("faultMsg"));
    			}
    			// Get the node "bpmn:userId" and test it has always an InMsg attribute
    			Node userId = ((Element) operation).getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0", "userId").item(0);
    			bpmnUserId = new Properties();
    			if (userId != null) {
    				// set the bpmnUserId properties
    				if ( ((Element) userId).getAttribute("inMsg") != null && ((Element) userId).getAttribute("inMsg") != "" )
    					bpmnUserId.put("inMsg",((Element) userId).getAttribute("inMsg"));
    				else throw new PEtALSCDKException("Malformed Wsdl: bpmn:userId attribute inMsg is mandatory "
    						+ " in wsdl operation = " + ((Element) operation).getAttribute("name"));
    				if ( ((Element) userId).getAttribute("outMsg") != null && ((Element) userId).getAttribute("outMsg") != "" )
    					bpmnUserId.put("outMsg",((Element) userId).getAttribute("outMsg"));
    				if ( ((Element) userId).getAttribute("faultMsg") != null && ((Element) userId).getAttribute("faultMsg") != "" )
    					bpmnUserId.put("faultMsg",((Element) userId).getAttribute("faultMsg"));
    			}
    			// Get the list of nodes "bpmn:variable"
    			NodeList bpmnVariableList = ((Element) operation).getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0", "variable");
    			bpmnVarInMsg = new Properties();
    			outMsgBpmnVar = new Properties();
    			faultMsgBpmnVar = new Properties();
    			bpmnVarList = new HashSet<String>();
    			for (int k=0; k < bpmnVariableList.getLength(); k++) {
    				Node bpmnVariable = bpmnVariableList.item(k);
    				// test name declaration of variable
    				if ( ((Element) bpmnVariable).getAttribute("bpmn") == null )
    	    			throw new PEtALSCDKException("Malformed Wsdl: bpmn:variable declared with no bpmn name in wsdl:operation: "
    	    					+ eptAndOperation.getOperationName() );
    				// test unicity of declared bpmnVariable
    				if ( bpmnVarList.contains(((Element) bpmnVariable).getAttribute("bpmn")) )
    	    			throw new PEtALSCDKException("Malformed Wsdl: bpmn:variable bpmn = " + ((Element) bpmnVariable).getAttribute("bpmn")
    	    					+ " declared twice in wsdl:operation: " + eptAndOperation.getOperationName() );
    				// Add bpmnVariables in the bpmnVarList
    				bpmnVarList.add(((Element) bpmnVariable).getAttribute("bpmn"));
    				// Add bpmnVariables
    				if ( ((Element) bpmnVariable).getAttribute("inMsg") != null && ((Element) bpmnVariable).getAttribute("inMsg") != "" )
    					bpmnVarInMsg.put(((Element) bpmnVariable).getAttribute("bpmn"), ((Element) bpmnVariable).getAttribute("inMsg"));
    				if ( ((Element) bpmnVariable).getAttribute("outMsg") != null && ((Element) bpmnVariable).getAttribute("outMsg") != "" )
    					outMsgBpmnVar.put(((Element) bpmnVariable).getAttribute("outMsg"), ((Element) bpmnVariable).getAttribute("bpmn"));
    				if ( ((Element) bpmnVariable).getAttribute("faultMsg") != null && ((Element) bpmnVariable).getAttribute("faultMsg") != "" )
    					faultMsgBpmnVar.put(((Element) bpmnVariable).getAttribute("faultMsg"), ((Element) bpmnVariable).getAttribute("bpmn"));
    			}
   		        // get the bpmn variable types from BpmnModel 
   		        BpmnModel model = repS.getBpmnModel(processDefinitionId);
   		        // getting list process from model including tasks
   		        bpmnVarType = new ConcurrentHashMap<String, org.activiti.bpmn.model.FormProperty>();
   		        List<org.activiti.bpmn.model.Process> processes = model.getProcesses();
   		        List<org.activiti.bpmn.model.FormProperty> formPropertyList = null;
				Boolean found = false;
				if ( bpmnActionType == BpmnActionType.START_EVENT) {
   					// search form Property for the Start Event: bpmnAction
					outerloop:
					for(org.activiti.bpmn.model.Process process : processes) {
						for(org.activiti.bpmn.model.FlowElement flowElt : process.getFlowElements()) {
           					// search the Start Event: bpmnAction
       						if( (flowElt instanceof StartEvent) && (flowElt.getId().equals(bpmnAction)) ) {
       							StartEvent startEvent = (StartEvent)flowElt;
       							formPropertyList = startEvent.getFormProperties();
       							found = true;
       							break outerloop;
       						}
						}
					}
				} else if ( bpmnActionType == BpmnActionType.USER_TASK) {
   					// search form Property for the User Task: bpmnAction
					outerloop:
					for(org.activiti.bpmn.model.Process process : processes) {
						for(org.activiti.bpmn.model.FlowElement flowElt : process.getFlowElements()) {
           					// search the Start Event: bpmnAction
       						if( (flowElt instanceof UserTask) && (flowElt.getId().equals(bpmnAction)) ) {
       							UserTask userTask = (UserTask)flowElt;
       							formPropertyList = userTask.getFormProperties();
       							found = true;
      							break outerloop;
       						}
						}
					}
    			}
				if (! found)
	    			throw new PEtALSCDKException("Malformed Wsdl: BpmnAction : " + bpmnAction  
	    					+ ", does not exist in Activiti process : " + processDefinitionId );
				if ( formPropertyList != null && formPropertyList.size() >0 ) {
      				for( org.activiti.bpmn.model.FormProperty formPropertie : formPropertyList) {
    					// add the FormProperty to the Map <bpmnvar, FormProperty>
       					bpmnVarType.put(formPropertie.getId(), formPropertie);
   		        	}
   		        }
    			//TODO test the existence of the declared bpmn:variables
				for(String bpmnVar : bpmnVarList) 
					if ( ! bpmnVarType.containsKey(bpmnVar) )
		    			throw new PEtALSCDKException("Malformed Wsdl: bpmn:variable : " + bpmnVar  
		    					+ ", does not exist in Activiti process : " + processDefinitionId );
				
    			// Create an activitiOperation
    			activitiOperation = new ActivitiOperation ( processDefinitionId, bpmnProcessKey, bpmnAction, bpmnActionType,
    					bpmnProcessId, bpmnUserId, bpmnVarInMsg, outMsgBpmnVar, faultMsgBpmnVar, bpmnVarType);
    			// Store the ActivitiOperation in the map with the corresponding end-point and Operation
    			((ActivitiSE) this.component).registerActivitiOperation(eptAndOperation, activitiOperation);
    		}
    	}
    	catch(TransformerException ex) {
    		throw new PEtALSCDKException("TransformerException ===" + ex);
    	} 
  
    	// List the map of (end-point and Operation) and ActivitiOperation
    	if( logger.isLoggable( Level.FINEST ))
    		((ActivitiSE) this.component).logEptOperationToActivitiOperation(logger, Level.FINEST);
	}
    
}
