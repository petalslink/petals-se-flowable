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
package org.ow2.petals.activitibpmn.operation;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;

import org.activiti.bpmn.model.FormProperty;
import org.activiti.bpmn.model.FormValue;
import org.activiti.engine.IdentityService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.TaskService;
import org.ow2.petals.activitibpmn.operation.exception.InvalidAnnotationException;
import org.ow2.petals.activitibpmn.operation.exception.NoUserIdMappingException;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

public abstract class ActivitiOperation {

    protected final String processDefinitionId;

    protected final String processKey;

    protected final String bpmnAction;

    protected final Properties bpmnProcessId;

    protected final Properties bpmnUserId;

    protected final Properties bpmnVarInMsg;

    protected final Properties outMsgBpmnVar;

    protected final Properties faultMsgBpmnVar;

    protected final Map<String, org.activiti.bpmn.model.FormProperty> bpmnVarType;

    protected final Logger logger;

    protected ActivitiOperation(final String processDefinitionId, final String processKey, final String bpmnAction,
            final Properties bpmnProcessId, final Properties bpmnUserId, final Properties bpmnVarInMsg,
            final Properties outMsgBpmnVar, final Properties faultMsgBpmnVar,
            final Map<String, org.activiti.bpmn.model.FormProperty> bpmnVarType, final Logger logger)
            throws InvalidAnnotationException {

        this.verifyParameters(processDefinitionId, processKey, bpmnAction, bpmnProcessId, bpmnUserId, bpmnVarInMsg,
                outMsgBpmnVar, faultMsgBpmnVar, bpmnVarType);

        this.processDefinitionId = processDefinitionId;
        this.processKey = processKey;
        this.bpmnAction = bpmnAction;
        this.bpmnProcessId = bpmnProcessId;
        this.bpmnUserId = bpmnUserId;
        this.bpmnVarInMsg = bpmnVarInMsg;
        this.outMsgBpmnVar = outMsgBpmnVar;
        this.faultMsgBpmnVar = faultMsgBpmnVar;
        this.bpmnVarType = bpmnVarType;
        this.logger = logger;
    }

    /**
     * Verify that annotation read from the WSDL are valid for the operation, otherwise the exception
     * {@link InvalidAnnotationException} is thrown.
     */
    protected void verifyParameters(final String processDefinitionId, final String processKey,
            final String bpmnAction, final Properties bpmnProcessId, final Properties bpmnUserId,
            final Properties bpmnVarInMsg, final Properties outMsgBpmnVar, final Properties faultMsgBpmnVar,
            final Map<String, org.activiti.bpmn.model.FormProperty> bpmnVarType) throws InvalidAnnotationException {

        // The mapping defining the process instance id is required to complete a user task
        final String userIdMapping = bpmnUserId.getProperty("inMsg");
        if (userIdMapping == null || userIdMapping.isEmpty()) {
            throw new NoUserIdMappingException(this);
        }

    }

    /**
     * @return The name of the BPMN action
     */
    public abstract String getName();

    /**
     * Execute the operation
     */
    public final String execute(final Document inMsgWsdl, final TaskService taskService,
            final IdentityService identityService, final RuntimeService runtimeService)
            throws MessagingException {

        if (logger.isLoggable(Level.FINE)) {
            logger.fine("Activiti processDefId = " + processDefinitionId);
            logger.fine("Activiti Action (TaskId) = " + bpmnAction);
            logger.fine("Activiti ActionType = " + this.getName());
        }

        inMsgWsdl.getDocumentElement().normalize();

        // Get the userId
        String varNameInMsg = bpmnUserId.getProperty("inMsg");
        final String bpmnUserIdValue;
        Node varNode = inMsgWsdl.getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0/su", varNameInMsg)
                .item(0);
        if (varNode == null) {
            throw new MessagingException("The bpmnUserId is mandatory and must be given through the message variable: "
                    + varNameInMsg + " for the task: " + bpmnAction + " of process: " + processDefinitionId + " !");
        } else {
            bpmnUserIdValue = varNode.getTextContent().trim();
        }

        if (logger.isLoggable(Level.FINE)) {
            logger.fine("bpmnUserId => InMsg = " + varNameInMsg + " - value = " + bpmnUserIdValue);
        }

        // Get the bpmn variables
        final Map<String, Object> processVars = new HashMap<String, Object>();
        for (final String varBpmn : bpmnVarInMsg.stringPropertyNames()) {
            varNameInMsg = bpmnVarInMsg.getProperty(varBpmn);
            varNode = inMsgWsdl.getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0/su", varNameInMsg)
                    .item(0);
            // test if the process variable is required and value is not present
            if (varNode == null) {
                if (bpmnVarType.get(varBpmn).isRequired()) {
                    throw new MessagingException("The task: " + bpmnAction + " of process: " + processDefinitionId
                            + " required a value of bpmn variables: " + varBpmn
                            + " that must be given through the message variable: " + varNameInMsg + " !");
                } else { // (! bpmnVarType.get(varBpmn).isRequired() )

                    if (logger.isLoggable(Level.FINEST)) {
                        logger.finest("bpmnVar: " + varBpmn + "=> InMsg: " + varNameInMsg + " => no value");
                    }

                    continue;
                }
            }
            final String varValueInMsg = varNode.getTextContent().trim();
            if ((varValueInMsg == null) || (varValueInMsg.isEmpty())) {
                if (bpmnVarType.get(varBpmn).isRequired()) {
                    throw new MessagingException("The task: " + bpmnAction + " of process: " + processDefinitionId
                            + " required a value of bpmn variables: " + varBpmn
                            + " that must be given through the message variable: " + varNameInMsg + " !");
                } else { // (! bpmnVarType.get(varBpmn).isRequired() )

                    if (logger.isLoggable(Level.FINE)) {
                        logger.fine("bpmnVar: " + varBpmn + "=> InMsg: " + varNameInMsg + " => no value");
                    }
                    continue;
                }
            }

            if (logger.isLoggable(Level.FINE)) {
                logger.fine("bpmnVar: " + varBpmn + "=> InMsg: " + varNameInMsg + " => value: " + varValueInMsg);
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
                            + " does not belong to the enum of Activiti variable " + varNameInMsg + " !");
                } else {
                    processVars.put(varBpmn, varValueInMsg);
                }
            } else if (varType.equals("date")) {
                try {
                    final SimpleDateFormat sdf = new SimpleDateFormat(bpmnVarType.get(varBpmn).getDatePattern());
                    processVars.put(varBpmn, (Date) sdf.parse(varValueInMsg));
                } catch (final ParseException e) {
                    throw new MessagingException("The value of " + varNameInMsg
                            + " must be a valid Date with date pattern " + bpmnVarType.get(varBpmn).getDatePattern()
                            + "!");
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

        final String bpmnProcessIdValue = this.doExecute(inMsgWsdl, taskService, identityService, runtimeService,
                bpmnUserIdValue, processVars);

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

        return sb.toString();
    }
    
    protected abstract String doExecute(final Document inMsgWsdl, final TaskService taskService,
            final IdentityService identityService, final RuntimeService runtimeService, final String bpmnUserId,
            final Map<String, Object> processVars)
            throws MessagingException;

    /**
     * @param logLevel
     */
    public void log(final Logger logger, final Level logLevel) {
        if (logger.isLoggable(logLevel)) {
            logger.log(logLevel, "operation '" + this.getClass().getSimpleName() + "':");
            logger.log(logLevel, "  - processDefinitionId = " + this.processDefinitionId);
            logger.log(logLevel, "  - processKey = " + this.processKey);
            logger.log(logLevel, "  - bpmnAction = " + this.bpmnAction);
            logger.log(
                    logLevel,
                    "  - bpmnProcessId: InMsg = " + this.bpmnProcessId.getProperty("inMsg") + " | outMsg = "
                            + this.bpmnProcessId.getProperty("outMsg") + " | faultMsg = "
                            + this.bpmnProcessId.getProperty("faultMsg"));
            logger.log(
                    logLevel,
                    "  - bpmnUserId: InMsg = " + this.bpmnUserId.getProperty("inMsg") + " | outMsg = "
                            + this.bpmnUserId.getProperty("outMsg") + " | faultMsg = "
                            + this.bpmnUserId.getProperty("faultMsg"));
            for (final Entry<Object, Object> entry : this.bpmnVarInMsg.entrySet()) {
                final String key = (String) entry.getKey();
                logger.log(logLevel, "  - bpmnVar => inMsg: " + key + " => " + entry.getValue());
            }
            for (final Entry<Object, Object> entry : this.outMsgBpmnVar.entrySet()) {
                final String key = (String) entry.getKey();
                logger.log(logLevel, "  - outMsg => bpmnVar: " + key + " => " + entry.getValue());
            }
            for (final Entry<Object, Object> entry : this.faultMsgBpmnVar.entrySet()) {
                final String key = (String) entry.getKey();
                logger.log(logLevel, "  - faultMsg => bpmnVar: " + key + " => " + entry.getValue());
            }
            logger.log(logLevel, "  - Activiti variable types");
            for (final Entry<String, FormProperty> entry : this.bpmnVarType.entrySet()) {
                final String key = entry.getKey();
                final FormProperty value = entry.getValue();
                logger.log(logLevel, "      - bpmn variable : " + key + " - Name = " + value.getName() + " - Type = "
                        + value.getType());
                if (value.getType().equals("enum")) {
                    for (final FormValue enumValue : value.getFormValues())
                        logger.log(logLevel, "        |------  enum value Id = " + enumValue.getId() + " - Value = "
                                + enumValue.getName());
                } else if (value.getType().equals("date")) {
                    logger.log(logLevel, "        |------  Date pattern = " + value.getDatePattern());
                }
            }
        }
    }

    public String getProcessDefinitionId() {
        return this.processDefinitionId;
    }

    public String getProcessKey() {
        return this.processKey;
    }

    public String getBpmnAction() {
        return this.bpmnAction;
    }

    public Properties getBpmnProcessId() {
        return this.bpmnProcessId;
    }

    public Properties getBpmnUserId() {
        return this.bpmnUserId;
    }

    public Properties getBpmnVarInMsg() {
        return this.bpmnVarInMsg;
    }

    public Properties getOutMsgBpmnVar() {
        return this.outMsgBpmnVar;
    }

    public Properties getFaultMsgBpmnVar() {
        return this.faultMsgBpmnVar;
    }

    public Map<String, org.activiti.bpmn.model.FormProperty> getBpmnVarType() {
        return this.bpmnVarType;
    }

}
