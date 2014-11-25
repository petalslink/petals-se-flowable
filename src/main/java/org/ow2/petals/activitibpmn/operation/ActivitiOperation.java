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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.StringWriter;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessagingException;
import javax.xml.bind.DatatypeConverter;
import javax.xml.namespace.QName;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;

import org.activiti.bpmn.model.FormProperty;
import org.activiti.bpmn.model.FormValue;
import org.ow2.petals.activitibpmn.operation.annotated.AnnotatedOperation;
import org.ow2.petals.activitibpmn.operation.exception.NoUserIdValueException;
import org.ow2.petals.activitibpmn.operation.exception.OperationProcessingException;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.w3c.dom.Document;

import com.ebmwebsourcing.easycommons.xml.DocumentBuilders;
import com.ebmwebsourcing.easycommons.xml.Transformers;

public abstract class ActivitiOperation {

    /**
     * Namespace of special parameters for the output XSLT style-sheet
     */
    protected static final String SCHEMA_OUTPUT_XSLT_SPECIAL_PARAMS = "http://petals.ow2.org/se/bpmn/output-params/1.0/special";

    /**
     * Local part of the special parameter name about the process instance identifier for the output XSLT style-sheet
     */
    protected static final String SCHEMA_OUTPUT_XSLT_PARAM_PROCESS_INSTANCE_ID = "processInstanceId";

    /**
     * Local part of the special parameter name about the user identifier for the output XSLT style-sheet
     */
    protected static final String SCHEMA_OUTPUT_XSLT_PARAM_USER_ID = "userId";

    /**
     * Namespace of process instance parameters for the output XSLT style-sheet
     */
    protected static final String SCHEMA_OUTPUT_XSLT_PROCESS_INSTANCE_PARAMS = "http://petals.ow2.org/se/bpmn/output-params/1.0/process-instance";

    /**
     * Namespace of task parameters for the output XSLT style-sheet
     */
    protected static final String SCHEMA_OUTPUT_XSLT_TASK_PARAMS = "http://petals.ow2.org/se/bpmn/output-params/1.0/task";

    /**
     * The WSDL operation name associated to this {@link ActivitiOperation}
     */
    protected final String wsdlOperationName;

    /**
     * The process definition identifier
     */
    protected final String processDefinitionId;

    /**
     * The identifier of the deployed process definition, different from the process definition identifier.
     */
    protected String deployedProcessDefinitionId = null;

    /**
     * The task identifier on which the action must be realize on the BPMN process side
     */
    protected final String actionId;

    /**
     * The compiled XPath expression of the process instance identifier placeholder
     */
    protected final XPathExpression proccesInstanceIdXPathExpr;

    /**
     * The compiled XPath expression of the user identifier placeholder
     */
    protected final XPathExpression userIdXPathExpr;

    /**
     * The definition of variables of the operation
     */
    protected final Map<String, XPathExpression> variables;

    /**
     * Types of variables
     */
    protected final Map<String, FormProperty> variableTypes;

    /**
     * The output XSLT style-sheet compiled
     */
    private final Templates outputTemplate;

    protected final Logger logger;

    /**
     * @param annotatedOperation
     *            Annotations of the operation to create
     * @param processDefinitionId
     *            The process definition identifier to associate to the operation to create
     * @param logger
     */
    protected ActivitiOperation(final AnnotatedOperation annotatedOperation, final Logger logger) {

        this.wsdlOperationName = annotatedOperation.getWsdlOperationName();
        this.processDefinitionId = annotatedOperation.getProcessDefinitionId();
        this.actionId = annotatedOperation.getActionId();
        this.proccesInstanceIdXPathExpr = annotatedOperation.getProcessInstanceIdHolder();
        this.userIdXPathExpr = annotatedOperation.getUserIdHolder();
        this.variables = annotatedOperation.getVariables();
        this.variableTypes = annotatedOperation.getVariableTypes();
        this.outputTemplate = annotatedOperation.getOutputTemplate();
        this.logger = logger;
    }

    /**
     * @param deployedProcessDefinitionId
     *            The identifier of the deployed process definition, different from the process definition identifier.
     */
    public void setDeployedProcessDefinitionId(final String deployedProcessDefinitionId) {
        this.deployedProcessDefinitionId = deployedProcessDefinitionId;
    }

    /**
     * @return The action to realize on the BPMN process side (ie, the name of the BPMN action)
     */
    public abstract String getAction();

    /**
     * Execute the operation
     */
    public final Source execute(final Exchange exchange) throws MessagingException {

        final Document inMsgWsdl = exchange.getInMessageContentAsDocument();
        final DOMSource domSource = new DOMSource(inMsgWsdl);

        if (this.logger.isLoggable(Level.FINE)) {
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
            this.logger.fine("*** inMsgWsdl = " + writer.toString());
        }

        if (this.logger.isLoggable(Level.FINE)) {
            this.logger.fine("Activiti processDefId = " + processDefinitionId);
            this.logger.fine("Activiti Action = " + this.getClass().getSimpleName());
            this.logger.fine("Activiti ActionType (TaskId) = " + this.getAction());
        }

        inMsgWsdl.getDocumentElement().normalize();

        // Get the userId
        final String userId;
        try {
            userId = this.userIdXPathExpr.evaluate(domSource);
            if (userId == null || userId.trim().isEmpty()) {
                throw new NoUserIdValueException(this.wsdlOperationName);
            }

            if (this.logger.isLoggable(Level.FINE)) {
                this.logger.fine("User identifier value: " + userId);
            }
        } catch (final XPathExpressionException e) {
            throw new OperationProcessingException(this.wsdlOperationName, e);
        }

        // Get the bpmn variables
        final Map<String, Object> variableValues = new HashMap<String, Object>();
        for (final Entry<String, XPathExpression> variable : this.variables.entrySet()) {
            final String variableName = variable.getKey();
            try {
                final String variableValueAsStr = variable.getValue().evaluate(domSource);
                if (variableValueAsStr == null || variableValueAsStr.trim().isEmpty()) {
                    if (this.variableTypes.get(variableName).isRequired()) {
                        throw new MessagingException("The task: " + this.getClass().getSimpleName() + " of process: "
                                + this.processDefinitionId + " required the variable: " + variableName);
                    } else {
                        if (logger.isLoggable(Level.FINE)) {
                            logger.fine("variable: " + variableName + "=> no value");
                        }
                    }
                } else {
                    if (logger.isLoggable(Level.FINE)) {
                        logger.fine("variable: " + variableName + "=> value: " + variableValueAsStr);
                    }

                    // Get the type of the bpmn variable
                    final FormProperty variableProperties = this.variableTypes.get(variableName);
                    final String varType = variableProperties.getType();
                    // Put the value in Map of activiti variable in the right format
                    if (varType.equals("string")) {
                        variableValues.put(variableName, variableValueAsStr);
                    } else if (varType.equals("long")) {
                        try {
                            variableValues.put(variableName, Long.valueOf(variableValueAsStr));
                        } catch (final NumberFormatException e) {
                            throw new MessagingException("The value of the variable '" + variableName
                                    + "' must be a long ! Current value is: " + variableValueAsStr);
                        }
                    } else if (varType.equals("enum")) {
                        boolean validValue = false;
                        for (final FormValue enumValue : variableProperties.getFormValues()) {
                            if (variableValueAsStr.equals(enumValue.getId())) {
                                validValue = true;
                            }
                        }
                        if (!validValue) {
                            throw new MessagingException("The value of the variable '" + variableName
                                    + " does not belong to the enum of Activiti variable ! Current value is: "
                                    + variableValueAsStr);
                        } else {
                            variableValues.put(variableName, variableValueAsStr);
                        }
                    } else if (varType.equals("date")) {
                        try {
                            variableValues.put(variableName, DatatypeConverter.parseDateTime(variableValueAsStr)
                                    .getTime());
                        } catch (final IllegalArgumentException e) {
                            throw new MessagingException("The value of the variable '" + variableName
                                    + "' must be a valid date ! Current value is: " + variableValueAsStr);
                        }
                    } else if (varType.equals("boolean")) {
                        if (variableValueAsStr.equalsIgnoreCase("true") || variableValueAsStr.equalsIgnoreCase("false")) {
                            variableValues.put(variableName, (Boolean) Boolean.valueOf(variableValueAsStr));
                        } else {
                            throw new MessagingException("The value of the variable '" + variableName
                                    + "' must be a boolean value \"true\" or \"false\" ! Current value is: "
                                    + variableValueAsStr);
                        }
                    }
                }
            } catch (final XPathExpressionException e) {
                throw new OperationProcessingException(this.wsdlOperationName, e);
            }
        }

        final Map<QName, String> xslParameters = new HashMap<QName, String>();
        this.doExecute(domSource, userId, variableValues, xslParameters);

        try {
            // TODO: For performance reasons, check that it is not needed to introduce a pool of transformers to avoid
            // to create a transformer on each request
            final Transformer outputTransformer = this.outputTemplate.newTransformer();

            // We prepare the parameters to set to the XSL transformer:
            if (this.logger.isLoggable(Level.INFO)) {
                this.logger.info("XSL Parameters:");
                for (final Entry<QName, String> variable : xslParameters.entrySet()) {
                    this.logger.info("\t- " + variable.getKey().toString() + " => " + variable.getValue());
                }
            }
            for (final Entry<QName, String> variable : xslParameters.entrySet()) {
                outputTransformer.setParameter(variable.getKey().toString(), variable.getValue());
            }

            // Create the output through the XSL transformation
            final ByteArrayOutputStream baos = new ByteArrayOutputStream();
            final Result result = new StreamResult(baos);
            // TODO: Improve the empty document to avoid to create it each time
            outputTransformer.transform(new DOMSource(DocumentBuilders.newDocument()), result);
            System.out.println(new String(baos.toByteArray()));
            return new StreamSource(new ByteArrayInputStream(baos.toByteArray()));
        } catch (final TransformerException e) {
            throw new OperationProcessingException(this.wsdlOperationName, e);
        }
    }
    
    /**
     * 
     * @param domSource
     *            The incoming XML payload
     * @param userId
     *            The user identifier
     * @param processVars
     * @param outputNamedValues
     *            The output named values to generate response
     * @throws MessagingException
     */
    protected abstract void doExecute(final DOMSource domSource, final String userId,
            final Map<String, Object> processVars, final Map<QName, String> outputNamedValues)
            throws MessagingException;

    /**
     * @param logLevel
     */
    public void log(final Logger logger, final Level logLevel) {
        if (logger.isLoggable(logLevel)) {
            logger.log(logLevel, "operation '" + this.getClass().getSimpleName() + "':");
            logger.log(logLevel, "  - processDefinitionId = " + this.processDefinitionId);
            logger.log(logLevel, "  - processInstanceId => compiled XPath expression");
            logger.log(logLevel, "  - action = " + this.getClass().getSimpleName());
            for (final String variableName : this.variables.keySet()) {
                logger.log(logLevel, "  - variable => name: " + variableName + " => compiled XPath expression");
            }
            logger.log(logLevel, "  - Activiti variable types");
            for (final Entry<String, FormProperty> entry : this.variableTypes.entrySet()) {
                final String key = entry.getKey();
                final FormProperty value = entry.getValue();
                logger.log(logLevel, "      - bpmn variable : " + key + " - Name = " + value.getName() + " - Type = "
                        + value.getType());
                if (value.getType().equals("enum")) {
                    for (final FormValue enumValue : value.getFormValues()) {
                        logger.log(logLevel, "        |------  enum value Id = " + enumValue.getId() + " - Value = "
                                + enumValue.getName());
                    }
                } else if (value.getType().equals("date")) {
                    logger.log(logLevel, "        |------  Date pattern = " + value.getDatePattern());
                }
            }
        }
    }

    public String getProcessDefinitionId() {
        return this.processDefinitionId;
    }

    /**
     * @return The WSDL operation name associated to this {@link ActivitiOperation}
     */
    public String getWsdlOperationName() {
        return this.wsdlOperationName;
    }

    /**
     * Convert a BPMN variable value to a XSL parameter value
     * 
     * @param bpmnVariableValue
     *            A BPMN variable value
     * @return The XSL parameter value
     */
    protected String convertBpmnVariableValueToXslParam(final Object bpmnVariableValue) {

        final String xslParamValue;
        if (bpmnVariableValue instanceof Date) {
            final Calendar calendar = Calendar.getInstance();
            calendar.setTime((Date) bpmnVariableValue);
            xslParamValue = DatatypeConverter.printDateTime(calendar);
        } else {
            xslParamValue = bpmnVariableValue.toString();
        }

        return xslParamValue;
    }

}
