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

import java.io.StringWriter;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.xml.bind.DatatypeConverter;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;

import org.activiti.bpmn.model.FormProperty;
import org.activiti.bpmn.model.FormValue;
import org.activiti.engine.IdentityService;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.TaskService;
import org.ow2.petals.activitibpmn.operation.annotated.AnnotatedOperation;
import org.ow2.petals.activitibpmn.operation.exception.NoUserIdValueException;
import org.ow2.petals.activitibpmn.operation.exception.OperationProcessingException;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.w3c.dom.Document;
import org.w3c.dom.Node;

import com.ebmwebsourcing.easycommons.xml.Transformers;

public abstract class ActivitiOperation {

    /**
     * The WSDL operation name associated to this {@link ActivitiOperation}
     */
    protected final String wsdlOperationName;

    protected final String processDefinitionId;

    protected final String processKey;

    protected final String bpmnAction;

    protected final Properties bpmnProcessId;

    protected final XPathExpression userIdXPathExpr;

    protected final Properties bpmnVarInMsg;

    protected final Properties outMsgBpmnVar;

    protected final Properties faultMsgBpmnVar;

    protected final Map<String, org.activiti.bpmn.model.FormProperty> bpmnVarType;

    protected final Logger logger;

    protected ActivitiOperation(final AnnotatedOperation annotatedOperation, final String processDefinitionId,
            final Map<String, org.activiti.bpmn.model.FormProperty> bpmnVarType, final Logger logger) {

        this.wsdlOperationName = annotatedOperation.getWsdlOperationName();
        this.processDefinitionId = processDefinitionId;
        this.processKey = annotatedOperation.getProcessIdentifier();
        this.bpmnAction = annotatedOperation.getBpmnAction();
        this.bpmnProcessId = annotatedOperation.getProcessInstanceIdHolder();
        this.userIdXPathExpr = annotatedOperation.getUserIdHolder();
        this.bpmnVarInMsg = annotatedOperation.getBpmnVarInMsg();
        this.outMsgBpmnVar = annotatedOperation.getOutMsgBpmnVar();
        this.faultMsgBpmnVar = annotatedOperation.getFaultMsgBpmnVar();
        this.bpmnVarType = bpmnVarType;
        this.logger = logger;
    }

    /**
     * @return The name of the BPMN action
     */
    public abstract String getBpmnActionType();

    /**
     * Execute the operation
     */
    public final String execute(final Exchange exchange, final TaskService taskService,
            final IdentityService identityService, final RuntimeService runtimeService)
            throws MessagingException {

        final Document inMsgWsdl = exchange.getInMessageContentAsDocument();
        final DOMSource domSource = new DOMSource(inMsgWsdl);
        final StringWriter writer = new StringWriter();
        final StreamResult result = new StreamResult(writer);
        final Transformer transformer = Transformers.takeTransformer();
        try {
            transformer.transform(domSource, result);
        } catch (final TransformerException e) {
            throw new MessagingException(e);
        } finally {
            Transformers.releaseTransformer(transformer);
        }
        if (logger.isLoggable(Level.FINE)) {
            logger.fine("*** inMsgWsdl = " + writer.toString());
        }


        if (logger.isLoggable(Level.FINE)) {
            logger.fine("Activiti processDefId = " + processDefinitionId);
            logger.fine("Activiti Action (TaskId) = " + bpmnAction);
            logger.fine("Activiti ActionType = " + this.getBpmnActionType());
        }

        inMsgWsdl.getDocumentElement().normalize();

        // Get the userId
        final String userId;
        try {
            userId = this.userIdXPathExpr.evaluate(domSource);
            if (userId == null || userId.trim().isEmpty()) {
                throw new NoUserIdValueException(this.wsdlOperationName);
            }

            if (logger.isLoggable(Level.FINE)) {
                logger.fine("bpmnUserId => InMsg = " + userId);
            }
        } catch (final XPathExpressionException e) {
            throw new OperationProcessingException(this.wsdlOperationName, e);
        }

        // Get the bpmn variables
        final Map<String, Object> processVars = new HashMap<String, Object>();
        for (final String varBpmn : bpmnVarInMsg.stringPropertyNames()) {
            String varNameInMsg = bpmnVarInMsg.getProperty(varBpmn);
            Node varNode = inMsgWsdl.getElementsByTagNameNS("http://petals.ow2.org/se/Activitibpmn/1.0/su",
                    varNameInMsg)
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
                    processVars.put(varBpmn, DatatypeConverter.parseDateTime(varValueInMsg).getTime());
                } catch (final IllegalArgumentException e) {
                    throw new MessagingException("The value of " + varNameInMsg + " must be a valid date !");
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
                userId, processVars);

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
    
    /**
     * 
     * @param inMsgWsdl
     * @param taskService
     * @param identityService
     * @param runtimeService
     * @param userId
     *            The user identifier
     * @param processVars
     * @return
     * @throws MessagingException
     */
    protected abstract String doExecute(final Document inMsgWsdl, final TaskService taskService,
            final IdentityService identityService, final RuntimeService runtimeService, final String userId,
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
