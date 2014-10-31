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

import java.io.ByteArrayInputStream;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.activiti.engine.IdentityService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.TaskService;
import org.activiti.engine.runtime.ProcessInstance;
import org.activiti.engine.task.Task;
import org.activiti.bpmn.model.FormValue;
import org.ow2.petals.activitibpmn.ActivitiSEConstants.BpmnActionType;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.component.framework.listener.AbstractJBIListener;
import org.w3c.dom.Document;
import org.w3c.dom.Node;


/**
 * Listens to messages incoming from inside Petals.
 * <p>
 * This class is in charge of processing messages coming from a Petals
 * consumer. These messages can be requests (in messages) or acknowledgments (ACK).
 * </p>
 * <p>
 * Depending on the invoked operation, the message exchange pattern (MEP) and
 * the component's logic, this class may build and send a response.
 * </p>
 * 
 * @author bescudie
 */
public class ActivitiJBIListener extends AbstractJBIListener {

    /**
     * The IdentityService of the ProcessEngine.
     * <p>
     * This IdentityService is initialized once <br />
     * </p>
     */
	private IdentityService identityService; 

    /**
     * The RuntimeService of the ProcessEngine.
     * <p>
     * This RuntimeService is initialized once <br />
     * </p>
     */
	private RuntimeService runTimeService; 
    /**
     * The TaskService of the ProcessEngine.
     * <p>
     * This TaskService is initialized once <br />
     * </p>
     */
	private TaskService taskService; 

	/**
     * The FormService of the ProcessEngine.
     * <p>
     * This FormService is initialized once <br />
     * </p>
     */
//	private FormService formService; 
	
	
    /**
     * The TaskService of the ProcessEngine.
     * <p>
     * This TaskService is initialized once <br />
     * </p>
     */
//	private TaskService taskService; 

//	private RepositoryService repositoryService; 
	
	
    /**
     * A prefix to use for logged messages.
     * <p>
     * This prefix is updated for every processed message.<br />
     * It only contains the exchange ID.
     * </p>
     */
    private String logHint;

    /*
     * (non-Javadoc)
     * @see org.ow2.petals.component.framework.listener.AbstractJBIListener
     */
    public void init() {
		// Initialize the identityService if necessary
		if ( identityService == null) identityService = ((ActivitiSE) this.component).getProcessEngine().getIdentityService();
 		// Initialize the runTimeService if necessary
		if ( runTimeService == null) runTimeService = ((ActivitiSE) this.component).getProcessEngine().getRuntimeService();
 		// Initialize the taskService if necessary
		if ( taskService == null) taskService = ((ActivitiSE) this.component).getProcessEngine().getTaskService();
 		// Initialize the repositoryService if necessary
//		if ( repositoryService == null) repositoryService = ((ActivitiSE) this.component).getProcessEngine().getRepositoryService();
		// Initialize the formService if necessary
//		if ( formService == null) formService = ((ActivitiSE) this.component).getProcessEngine().getFormService();

    }

    /*
     * (non-Javadoc)
     * @see org.ow2.petals.component.framework.listener.AbstractJBIListener
     * #onJBIMessage(org.ow2.petals.component.framework.api.message.Exchange)
     */
    @Override
	public boolean onJBIMessage( Exchange exchange ) {
 
        Logger logger = getLogger();
    	logger.info("***********************");		
    	logger.info("*** Start onJBIMessage() in ActivitiJBIListener");
        Exception finalException = null;

        if (exchange.isActiveStatus()) {
        	this.logHint = "Exchange " + exchange.getExchangeId();
        	try {
        		// Get the InMessage
            	final NormalizedMessage normalizedMessage = exchange.getInMessage();
        		if( logger.isLoggable( Level.FINEST ))
        			logger.finest("normalizedMessage = " + normalizedMessage.toString() );

        		// Provider role 
        		if( exchange.isProviderRole()) {
                     
        			// Validate Message pattern
        			if(  ! exchange.isInOutPattern()) {
        				if( logger.isLoggable( Level.INFO ))
        					logger.info( this.logHint + " encountered a problem. The exchange pattern must be IN/OUT !" );
        	            throw new MessagingException( "The exchange pattern must be IN/OUT !" );
        				}

        			// TODO Validate Message

        			
        			// Get the eptName and Operation
        			String eptName = exchange.getEndpointName();
        			String operationName = exchange.getOperationName();
        			// Set eptAndoperation
        			EptAndOperation eptAndOperation	= new EptAndOperation(eptName, operationName);

        			if( logger.isLoggable( Level.FINEST )) {
        				logger.finest(logHint + " was received and is started to be processed.");
        				logger.finest("interfaceName = " + exchange.getInterfaceName());
        				logger.finest("Service       = " + exchange.getService());
        				logger.finest("EndpointName  = " + eptName );
        				logger.finest("OperationName = " + operationName );
        				logger.finest("Pattern " + exchange.getPattern() );
        			}
 
        			// Get the Activiti Operation  from the Map eptOperationToActivitiOperation
        			// @see org.ow2.petals.activitibpmn.ActivitiSE
        			ActivitiOperation activitiOperation = ((ActivitiSE) this.component).getActivitiOperations(eptAndOperation);
        			String processDefinitionId = activitiOperation.getProcessDefinitionId();
        			String bpmnAction = activitiOperation.getBpmnAction();
        			BpmnActionType bpmnActionType= activitiOperation.getBpmnActionType();
        			Properties bpmnProcessId = activitiOperation.getBpmnProcessId();
	                Properties bpmnUserId =activitiOperation.getBpmnUserId();
	                Properties bpmnVarInMsg = activitiOperation.getBpmnVarInMsg();
	                Properties outMsgBpmnVar = activitiOperation.getOutMsgBpmnVar();
	                Properties faultMsgBpmnVar = activitiOperation.getFaultMsgBpmnVar();
	            	Map<String, org.activiti.bpmn.model.FormProperty > bpmnVarType = activitiOperation.getBpmnVarType() ;

        			if( logger.isLoggable( Level.FINEST )) {
        				logger.finest("Activiti processDefId = " + processDefinitionId);
        				logger.finest("Activiti Action (TaskId) = " + bpmnAction);
        				logger.finest("Activiti ActionType = " + bpmnActionType);
        			}
 
        			// Get the exchange data
	                Document inMsgWsdl = exchange.getInMessageContentAsDocument();
	                DOMSource domSource = new DOMSource(inMsgWsdl);
	        		StringWriter writer = new StringWriter();
	        		StreamResult result = new StreamResult(writer);
	        		TransformerFactory tf = TransformerFactory.newInstance();
	        		Transformer transformer = tf.newTransformer();
	        		transformer.transform(domSource, result);
	        		inMsgWsdl.getDocumentElement().normalize();

	       			if( logger.isLoggable( Level.FINEST ))
	       				logger.finest("*** inMsgWsdl = " + writer.toString() );

	                String varNameInMsg;
	                String varValueInMsg;
        			Node varNode;
        			// Get the processId
        			varNameInMsg = bpmnProcessId.getProperty("inMsg");
        			String bpmnProcessIdValue = "";
        			if ( bpmnActionType == BpmnActionType.USER_TASK ) { //( varNameInMsg != null)
        				varNode = inMsgWsdl.getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0/su", varNameInMsg).item(0);
            			if (varNode == null)
            				throw new MessagingException( "The bpmnProcessId is mandatory and must be given through the message variable: "
            						+ varNameInMsg + " for the userTask: " + bpmnAction + " of process: " + processDefinitionId +" !");
            			else
            				bpmnProcessIdValue = varNode.getTextContent().trim();

        				if( logger.isLoggable( Level.FINEST ))
        					logger.finest("bpmnProcessId => InMsg = " + varNameInMsg + " - value = "+ bpmnProcessIdValue );

        			}
        		
           			// Get the userId
        			varNameInMsg = bpmnUserId.getProperty("inMsg");
        			String bpmnUserIdValue = "";
        			varNode = inMsgWsdl.getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0/su", varNameInMsg).item(0);
        			if (varNode == null)
        				throw new MessagingException( "The bpmnUserId is mandatory and must be given through the message variable: "
        						+ varNameInMsg + " for the task: " + bpmnAction + " of process: " + processDefinitionId +" !");
        			else {
        				bpmnUserIdValue = varNode.getTextContent().trim();
        			}
        			
        			if( logger.isLoggable( Level.FINEST ))
        				logger.finest("bpmnUserId => InMsg = " + varNameInMsg + " - value = "+ bpmnUserIdValue );
 
	        		// Get the bpmn variables
	        		Map<String, Object> processVars = new HashMap<String, Object>();
        			for(String varBpmn : bpmnVarInMsg.stringPropertyNames()) {
        				varNameInMsg = bpmnVarInMsg.getProperty(varBpmn);
    	        		varNode = inMsgWsdl.getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0/su", varNameInMsg).item(0);
    	        		// test if the process variable is required and value is not present 
    	        		if (varNode == null) {
    	        			if ( bpmnVarType.get(varBpmn).isRequired() )
    	        				throw new MessagingException( "The task: " + bpmnAction + " of process: " + processDefinitionId
    	        						+" required a value of bpmn variables: " + varBpmn 
    	        						+ " that must be given through the message variable: " + varNameInMsg + " !");
    	        			else { // (! bpmnVarType.get(varBpmn).isRequired() )
    	    	        		
    	    	       			if( logger.isLoggable( Level.FINEST ))
    	    	       				logger.finest("bpmnVar: " + varBpmn + "=> InMsg: " + varNameInMsg + " => no value" );
    	    	       			
    	        				continue;
    	        			}
    	        		}
    	        		varValueInMsg = varNode.getTextContent().trim();
    	        		if ( (varValueInMsg == null) || (varValueInMsg.isEmpty()) ) {
    	        			if ( bpmnVarType.get(varBpmn).isRequired() )
    	        				throw new MessagingException( "The task: " + bpmnAction + " of process: " + processDefinitionId
    	        						+" required a value of bpmn variables: " + varBpmn 
    	        						+ " that must be given through the message variable: " + varNameInMsg + " !");
    	        			else { // (! bpmnVarType.get(varBpmn).isRequired() )
    	    	        		
    	    	       			if( logger.isLoggable( Level.FINEST ))
    	    	       				logger.finest("bpmnVar: " + varBpmn + "=> InMsg: " + varNameInMsg + " => no value" );
    	    	       			
    	        				continue;
    	        			}
    	        		}
    	        		
    	       			if( logger.isLoggable( Level.FINEST ))
    	       				logger.finest("bpmnVar: " + varBpmn + "=> InMsg: " + varNameInMsg + " => value: " + varValueInMsg );
    	       			
    	       			// Get the type of the bpmn variable
    	       			String varType = bpmnVarType.get(varBpmn).getType();
    	       			// Put the value in Map of activiti variable in the right format
    	       			if (varType.equals("string")) {
    	       				processVars.put(varBpmn, varValueInMsg);
    	       			} else if (varType.equals("long")) {
    	       				try {
    	       					processVars.put(varBpmn, Long.valueOf(varValueInMsg));
    	       				} catch (NumberFormatException e) {
    	        	            throw new MessagingException( "The value of " + varNameInMsg + " must be a long !" );
    	       				}
       	       			} else if (varType.equals("enum")) {
       	       				Boolean validValue = false;
            				for (FormValue enumValue : bpmnVarType.get(varBpmn).getFormValues() ) 
            					if ( varValueInMsg.equals(enumValue.getId()) ) validValue = true;
            				if ( ! validValue ) 
            					throw new MessagingException( "The value of " + varNameInMsg 
            							+ " does not belong to the enum of Activiti variable " + varNameInMsg + " !");
            				else
            					processVars.put(varBpmn, varValueInMsg);
        				} else if (varType.equals("date")) {
    	       				try {
    	       					SimpleDateFormat sdf = new SimpleDateFormat(bpmnVarType.get(varBpmn).getDatePattern());
            					processVars.put(varBpmn, (Date) sdf.parse(varValueInMsg) );
    	       				} catch (ParseException e) {
    	        	            throw new MessagingException( "The value of " + varNameInMsg 
    	        	            		+ " must be a valid Date with date pattern " + bpmnVarType.get(varBpmn).getDatePattern() + "!" );
    	       				}
        				} else if (varType.equals("boolean")) {
        					if (varValueInMsg.equalsIgnoreCase("true") || varValueInMsg.equalsIgnoreCase("false")) 
        						processVars.put(varBpmn, (Boolean) Boolean.valueOf(varValueInMsg) );
        					else
    	        	            throw new MessagingException( "The value of " + varNameInMsg 
    	        	            		+ " must be a boolean value \"true\" or \"false\" !" );
        				}	

        			}
       			
                //TODO test if user is authorized with identityLink
        		//TODO why don't use FormService.submitTaskFormData(taskId, properties)
        			
        		if ( bpmnActionType == BpmnActionType.START_EVENT ) {
               		// Start a new process instance
            		// TODO Set the CategoryId (not automaticaly done, but automaticaly done for tenant_id ?)
            		try {
            			identityService.setAuthenticatedUserId(bpmnUserIdValue);
            			ProcessInstance processInstance = runTimeService.startProcessInstanceById(processDefinitionId, processVars);
            			bpmnProcessIdValue = processInstance.getId();
            		} finally {
            			identityService.setAuthenticatedUserId(null);
            		}
	        		
       			if( logger.isLoggable( Level.FINEST ))
       				logger.finest("*** NEW PROCESS INSTANCE started,  processId = " + bpmnProcessIdValue );
	       			
        		} else if ( bpmnActionType == BpmnActionType.USER_TASK) {
        			// Get the Task
            		List<Task> taskList = taskService.createTaskQuery().processInstanceId(bpmnProcessIdValue)
            				.taskDefinitionKey(bpmnAction).list();
            		if ( (taskList == null) || (taskList.size() == 0 ) )
        	            throw new MessagingException( "No tasks: " + bpmnAction + " for processInstance: " + bpmnProcessIdValue
        	            		+ " was found.");
            		// Perform the user Task
            		try {
            			identityService.setAuthenticatedUserId(bpmnUserIdValue);
                		taskService.complete(taskList.get(0).getId(), processVars); 
            		} finally {
            			identityService.setAuthenticatedUserId(null);
            		}
        		}
        		// Build the outMessage
        		
        		//TODO
        		
        		StringBuilder sb = new StringBuilder();
        		sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        		sb.append("<su:numero xmlns:su=\"http://petals.ow2.org/se/Activitibpmn/1.0/su\">\n");
        		sb.append("   <su:numeroDde>" + bpmnProcessIdValue +"</su:numeroDde>\n");
//        		sb.append("   <requestId>12458</requestId>\n");
        		sb.append("</su:numero>\n");
        		// Keep the machine's default encoding
        		exchange.setOutMessageContent(new ByteArrayInputStream(sb.toString().getBytes("UTF-8")));

        		}
        		
        	} catch (MessagingException e) {
            	finalException = e;
            } catch (UnsupportedEncodingException e) {
            	finalException = e;
			} catch (TransformerException e) {
            	finalException = e;
			}

            // Handle cases where a fault could not be set on the exchange
            if (finalException != null) {
            	logger.finest("Exchange " + exchange.getExchangeId() + " encountered a problem. " + finalException.getMessage());
                // Technical error, it would be set as a Fault by the CDK
            	exchange.setError(finalException);
            }

     		

        }
    	
    	// True to let the CDK close the exchange.
    	// False to explicitly return the exchange.
        return true;
    }


}
