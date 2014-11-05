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
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.jbi.messaging.NormalizedMessage;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.activiti.bpmn.model.FormValue;
import org.activiti.engine.IdentityService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.TaskService;
import org.activiti.engine.runtime.ProcessInstance;
import org.activiti.engine.task.Task;
import org.ow2.petals.activitibpmn.ActivitiSEConstants.BpmnActionType;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.component.framework.listener.AbstractJBIListener;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.ebmwebsourcing.easycommons.xml.Transformers;


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

    /**
     * {@inheritDoc}
     */
    public void init() {
        if (this.component instanceof ActivitiSE) {
            final ActivitiSE activitiComp = (ActivitiSE) this.component;

            // Initialize the identityService if necessary
            if (this.identityService == null) {
                this.identityService = activitiComp.getProcessEngine().getIdentityService();
            }
            // Initialize the runTimeService if necessary
            if (this.runTimeService == null) {
                this.runTimeService = activitiComp.getProcessEngine().getRuntimeService();
            }
            // Initialize the taskService if necessary
            if (this.taskService == null) {
                this.taskService = activitiComp.getProcessEngine().getTaskService();
            }
            // Initialize the repositoryService if necessary
            // if ( this.repositoryService == null) { this.repositoryService =
            // activitiComp.getProcessEngine().getRepositoryService(); }
            // Initialize the formService if necessary
            // if ( this.formService == null) { this.formService = activitiComp.getProcessEngine().getFormService(); }
        }

    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean onJBIMessage(final Exchange exchange) {
 
        final Logger logger = getLogger();
        logger.fine("Start ActivitiJBIListener.onJBIMessage()");
        try {
            Exception finalException = null;

            if (exchange.isActiveStatus()) {
                this.logHint = "Exchange " + exchange.getExchangeId();
                try {
                    // Get the InMessage
                    final NormalizedMessage normalizedMessage = exchange.getInMessage();
                    if (logger.isLoggable(Level.FINE))
                        logger.fine("normalizedMessage = " + normalizedMessage.toString());

                    // Provider role
                    if (exchange.isProviderRole()) {

                        // Validate Message pattern
                        if (!exchange.isInOutPattern()) {
                            if (logger.isLoggable(Level.WARNING)) {
                                logger.warning(this.logHint
                                        + " encountered a problem. The exchange pattern must be IN/OUT !");
                            }
                            throw new MessagingException("The exchange pattern must be IN/OUT !");
                        }

                        // TODO Validate Message

                        // Get the eptName and Operation
                        String eptName = exchange.getEndpointName();
                        String operationName = exchange.getOperationName();
                        // Set eptAndoperation
                        EptAndOperation eptAndOperation = new EptAndOperation(eptName, operationName);

                        if (logger.isLoggable(Level.FINE)) {
                            logger.fine(logHint + " was received and is started to be processed.");
                            logger.fine("interfaceName = " + exchange.getInterfaceName());
                            logger.fine("Service       = " + exchange.getService());
                            logger.fine("EndpointName  = " + eptName);
                            logger.fine("OperationName = " + operationName);
                            logger.fine("Pattern " + exchange.getPattern());
                        }

                        // Get the Activiti Operation from the Map eptOperationToActivitiOperation
                        // @see org.ow2.petals.activitibpmn.ActivitiSE
                        final ActivitiOperation activitiOperation = ((ActivitiSE) this.component)
                                .getActivitiOperations(eptAndOperation);
                        final String processDefinitionId = activitiOperation.getProcessDefinitionId();
                        final String bpmnAction = activitiOperation.getBpmnAction();
                        final BpmnActionType bpmnActionType = activitiOperation.getBpmnActionType();
                        final Properties bpmnProcessId = activitiOperation.getBpmnProcessId();
                        final Properties bpmnUserId = activitiOperation.getBpmnUserId();
                        final Properties bpmnVarInMsg = activitiOperation.getBpmnVarInMsg();
                        final Properties outMsgBpmnVar = activitiOperation.getOutMsgBpmnVar();
                        final Properties faultMsgBpmnVar = activitiOperation.getFaultMsgBpmnVar();
                        final Map<String, org.activiti.bpmn.model.FormProperty> bpmnVarType = activitiOperation
                                .getBpmnVarType();

                        if (logger.isLoggable(Level.FINE)) {
                            logger.fine("Activiti processDefId = " + processDefinitionId);
                            logger.fine("Activiti Action (TaskId) = " + bpmnAction);
                            logger.fine("Activiti ActionType = " + bpmnActionType);
                        }

                        // Get the exchange data
                        final Document inMsgWsdl = exchange.getInMessageContentAsDocument();
                        final DOMSource domSource = new DOMSource(inMsgWsdl);
                        final StringWriter writer = new StringWriter();
                        final StreamResult result = new StreamResult(writer);
                        final Transformer transformer = Transformers.takeTransformer();
                        try {
                            transformer.transform(domSource, result);
                        } finally {
                            Transformers.releaseTransformer(transformer);
                        }
                        inMsgWsdl.getDocumentElement().normalize();

                        if (logger.isLoggable(Level.FINE)) {
                            logger.fine("*** inMsgWsdl = " + writer.toString());
                        }

                        String varNameInMsg;
                        String varValueInMsg;
                        Node varNode;
                        // Get the processId
                        varNameInMsg = bpmnProcessId.getProperty("inMsg");
                        String bpmnProcessIdValue = "";
                        if (bpmnActionType == BpmnActionType.USER_TASK) { // ( varNameInMsg != null)
                            varNode = inMsgWsdl.getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0/su",
                                    varNameInMsg).item(0);
                            if (varNode == null) {
                                throw new MessagingException(
                                        "The bpmnProcessId is mandatory and must be given through the message variable: "
                                                + varNameInMsg + " for the userTask: " + bpmnAction + " of process: "
                                                + processDefinitionId + " !");
                            } else {
                                bpmnProcessIdValue = varNode.getTextContent().trim();
                            }

                            if (logger.isLoggable(Level.FINE)) {
                                logger.fine("bpmnProcessId => InMsg = " + varNameInMsg + " - value = "
                                        + bpmnProcessIdValue);
                            }
                        }

                        // Get the userId
                        varNameInMsg = bpmnUserId.getProperty("inMsg");
                        String bpmnUserIdValue = "";
                        varNode = inMsgWsdl.getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0/su",
                                varNameInMsg).item(0);
                        if (varNode == null) {
                            throw new MessagingException(
                                    "The bpmnUserId is mandatory and must be given through the message variable: "
                                            + varNameInMsg + " for the task: " + bpmnAction + " of process: "
                                            + processDefinitionId + " !");
                        }
 else {
                            bpmnUserIdValue = varNode.getTextContent().trim();
                        }

                        if (logger.isLoggable(Level.FINE)) {
                            logger.fine("bpmnUserId => InMsg = " + varNameInMsg + " - value = " + bpmnUserIdValue);
                        }

                        // Get the bpmn variables
                        final Map<String, Object> processVars = new HashMap<String, Object>();
                        for (final String varBpmn : bpmnVarInMsg.stringPropertyNames()) {
                            varNameInMsg = bpmnVarInMsg.getProperty(varBpmn);
                            varNode = inMsgWsdl.getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0/su",
                                    varNameInMsg).item(0);
                            // test if the process variable is required and value is not present
                            if (varNode == null) {
                                if (bpmnVarType.get(varBpmn).isRequired()) {
                                    throw new MessagingException("The task: " + bpmnAction + " of process: "
                                            + processDefinitionId + " required a value of bpmn variables: " + varBpmn
                                            + " that must be given through the message variable: " + varNameInMsg
                                            + " !");
                                } else { // (! bpmnVarType.get(varBpmn).isRequired() )

                                    if (logger.isLoggable(Level.FINEST)) {
                                        logger.finest("bpmnVar: " + varBpmn + "=> InMsg: " + varNameInMsg
                                                + " => no value");
                                    }

                                    continue;
                                }
                            }
                            varValueInMsg = varNode.getTextContent().trim();
                            if ((varValueInMsg == null) || (varValueInMsg.isEmpty())) {
                                if (bpmnVarType.get(varBpmn).isRequired()) {
                                    throw new MessagingException("The task: " + bpmnAction + " of process: "
                                            + processDefinitionId + " required a value of bpmn variables: " + varBpmn
                                            + " that must be given through the message variable: " + varNameInMsg
                                            + " !");
                                }
 else { // (! bpmnVarType.get(varBpmn).isRequired() )

                                    if (logger.isLoggable(Level.FINE)) {
                                        logger.fine("bpmnVar: " + varBpmn + "=> InMsg: " + varNameInMsg
                                                + " => no value");
                                    }
                                    continue;
                                }
                            }

                            if (logger.isLoggable(Level.FINE)) {
                                logger.fine("bpmnVar: " + varBpmn + "=> InMsg: " + varNameInMsg + " => value: "
                                        + varValueInMsg);
                            }

                            // Get the type of the bpmn variable
                            final String varType = bpmnVarType.get(varBpmn).getType();
                            // Put the value in Map of activiti variable in the right format
                            if (varType.equals("string")) {
                                processVars.put(varBpmn, varValueInMsg);
                            } else if (varType.equals("long")) {
                                try {
                                    processVars.put(varBpmn, Long.valueOf(varValueInMsg));
                                } catch (final NumberFormatException e) {
                                    throw new MessagingException("The value of " + varNameInMsg + " must be a long !");
                                }
                            } else if (varType.equals("enum")) {
                                boolean validValue = false;
                                for (final FormValue enumValue : bpmnVarType.get(varBpmn).getFormValues()) {
                                    if (varValueInMsg.equals(enumValue.getId())) {
                                        validValue = true;
                                    }
                                }
                                if (!validValue) {
                                    throw new MessagingException("The value of " + varNameInMsg
                                            + " does not belong to the enum of Activiti variable " + varNameInMsg
                                            + " !");
                                } else {
                                    processVars.put(varBpmn, varValueInMsg);
                                }
                            } else if (varType.equals("date")) {
                                try {
                                    final SimpleDateFormat sdf = new SimpleDateFormat(bpmnVarType.get(varBpmn)
                                            .getDatePattern());
                                    processVars.put(varBpmn, (Date) sdf.parse(varValueInMsg));
                                } catch (final ParseException e) {
                                    throw new MessagingException("The value of " + varNameInMsg
                                            + " must be a valid Date with date pattern "
                                            + bpmnVarType.get(varBpmn).getDatePattern() + "!");
                                }
                            } else if (varType.equals("boolean")) {
                                if (varValueInMsg.equalsIgnoreCase("true") || varValueInMsg.equalsIgnoreCase("false")) {
                                    processVars.put(varBpmn, (Boolean) Boolean.valueOf(varValueInMsg));
                                } else {
                                    throw new MessagingException("The value of " + varNameInMsg
                                            + " must be a boolean value \"true\" or \"false\" !");
                                }
                            }

                        }

                        // TODO test if user is authorized with identityLink
                        // TODO why don't use FormService.submitTaskFormData(taskId, properties)

                        if (bpmnActionType == BpmnActionType.START_EVENT) {
                            // Start a new process instance
                            // TODO Set the CategoryId (not automaticaly done, but automaticaly done for tenant_id ?)
                            try {
                                // TODO: How this works on concurrent requests. I think that it is not thread-safe ?
                                this.identityService.setAuthenticatedUserId(bpmnUserIdValue);
                                final ProcessInstance processInstance = this.runTimeService.startProcessInstanceById(
                                        processDefinitionId, processVars);
                                bpmnProcessIdValue = processInstance.getId();
                            } finally {
                                this.identityService.setAuthenticatedUserId(null);
                            }
    	        		
                            if (logger.isLoggable(Level.FINE)) {
                                logger.fine("*** NEW PROCESS INSTANCE started,  processId = " + bpmnProcessIdValue);
                            }
    	       			
                        } else if (bpmnActionType == BpmnActionType.USER_TASK) {
                            // Get the Task
                            final List<Task> taskList = this.taskService.createTaskQuery()
                                    .processInstanceId(bpmnProcessIdValue).taskDefinitionKey(bpmnAction).list();
                            if ((taskList == null) || (taskList.size() == 0)) {
                                throw new MessagingException("No tasks: " + bpmnAction + " for processInstance: "
                                        + bpmnProcessIdValue + " was found.");
                            }
                            // Perform the user Task
                            try {
                                this.identityService.setAuthenticatedUserId(bpmnUserIdValue);
                                this.taskService.complete(taskList.get(0).getId(), processVars);
                            } finally {
                                this.identityService.setAuthenticatedUserId(null);
                            }
            		}

                        // Build the outMessage
                        // TODO: The output should be compliant to the WSDL, not hard-coded
                        // TODO: Don't build XML using StringBuilder. Because of encoding problems, prefer to use DOM or
                        // equivalent
                        final StringBuilder sb = new StringBuilder();
                        sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
                        sb.append("<su:numero xmlns:su=\"http://petals.ow2.org/se/Activitibpmn/1.0/su\">\n");
                        sb.append("   <su:numeroDde>" + bpmnProcessIdValue + "</su:numeroDde>\n");
                        // sb.append("   <requestId>12458</requestId>\n");
                        sb.append("</su:numero>\n");
                        // Keep the machine's default encoding
                        exchange.setOutMessageContent(new ByteArrayInputStream(sb.toString().getBytes("UTF-8")));

            		}

                } catch (final MessagingException e) {
                    finalException = e;
                } catch (final UnsupportedEncodingException e) {
                    finalException = e;
                } catch (final TransformerException e) {
                    finalException = e;
                }

                // Handle cases where a fault could not be set on the exchange
                if (finalException != null) {
                    logger.finest("Exchange " + exchange.getExchangeId() + " encountered a problem. "
                            + finalException.getMessage());
                    // Technical error, it would be set as a Fault by the CDK
                    exchange.setError(finalException);
                }
            }

            // True to let the CDK close the exchange.
            // False to explicitly return the exchange.
            return true;
        } finally {
            logger.fine("End ActivitiJBIListener.onJBIMessage()");
        }
    }
}