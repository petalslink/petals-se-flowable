/**
 * Copyright (c) 2015-2020 Linagora
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
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.incoming.operation;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessagingException;
import javax.xml.namespace.QName;
import javax.xml.transform.Templates;
import javax.xml.transform.TransformerException;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;

import org.flowable.bpmn.model.FormValue;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.flowable.incoming.FlowableService;
import org.ow2.petals.flowable.incoming.operation.annotated.AnnotatedOperation;
import org.ow2.petals.flowable.incoming.operation.exception.InvalidMEPException;
import org.ow2.petals.flowable.incoming.operation.exception.NoUserIdValueException;
import org.ow2.petals.flowable.incoming.operation.exception.OperationProcessingException;
import org.ow2.petals.flowable.incoming.operation.exception.OperationProcessingFault;
import org.ow2.petals.flowable.incoming.variable.VariableDateImpl;
import org.ow2.petals.flowable.incoming.variable.VariableEnumImpl;
import org.ow2.petals.flowable.incoming.variable.VariableImpl;
import org.ow2.petals.flowable.incoming.variable.VariableImplBuilder;
import org.ow2.petals.flowable.incoming.variable.exception.VariableException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableUnsupportedTypeException;
import org.ow2.petals.flowable.incoming.variable.exception.VariableValueRequiredException;
import org.ow2.petals.flowable.utils.XslUtils;
import org.w3c.dom.Document;

import com.ebmwebsourcing.easycommons.xml.XMLPrettyPrinter;
import com.fasterxml.jackson.databind.ObjectMapper;

public abstract class FlowableOperation implements FlowableService {

    /**
     * Namespace of special parameters for the output XSLT style-sheet
     */
    public static final String SCHEMA_OUTPUT_XSLT_SPECIAL_PARAMS = "http://petals.ow2.org/se/flowable/output-params/1.0/special";

    /**
     * Local part of the special parameter name about the process instance identifier for the output XSLT style-sheet
     */
    public static final String SCHEMA_OUTPUT_XSLT_PARAM_PROCESS_INSTANCE_ID = "processInstanceId";

    /**
     * Local part of the special parameter name about the user identifier for the output XSLT style-sheet
     */
    public static final String SCHEMA_OUTPUT_XSLT_PARAM_USER_ID = "userId";

    /**
     * Local part of the special parameter name about the task identifier for the output XSLT style-sheet
     */
    public static final String SCHEMA_OUTPUT_XSLT_PARAM_TASK_ID = "taskId";

    /**
     * Local part of the special parameter name about the message name for the output XSLT style-sheet
     */
    public static final String SCHEMA_OUTPUT_XSLT_PARAM_MESSAGE_NAME = "messageName";

    /**
     * Namespace of process instance parameters for the output XSLT style-sheet
     */
    protected static final String SCHEMA_OUTPUT_XSLT_PROCESS_INSTANCE_PARAMS = "http://petals.ow2.org/se/flowable/output-params/1.0/process-instance";

    /**
     * Namespace of task parameters for the output XSLT style-sheet
     */
    protected static final String SCHEMA_OUTPUT_XSLT_TASK_PARAMS = "http://petals.ow2.org/se/flowable/output-params/1.0/task";

    /**
     * Namespace of fault parameters for the fault XSLT style-sheet
     */
    public static final String SCHEMA_OUTPUT_XSLT_FAULT_PARAMS = "http://petals.ow2.org/se/flowable/faults/1.0";

    /**
     * The WSDL operation name associated to this {@link FlowableOperation}
     */
    protected final QName wsdlOperation;

    /**
     * The process definition identifier
     */
    protected final String processDefinitionId;

    /**
     * The identifier of the deployed process definition, different from the process definition identifier.
     */
    protected String deployedProcessDefinitionId = null;

    /**
     * The compiled XPath expression of the user identifier placeholder
     */
    protected final XPathExpression userIdXPathExpr;

    /**
     * The variable implementations of the operation
     */
    protected final Map<String, VariableImpl> variables;

    /**
     * The output XSLT style-sheet compiled
     */
    private final Templates outputTemplate;

    /**
     * The XSLT style-sheet compiled associated to WSDL faults. The key is the class simple name of the exception
     * associated to the fault.
     */
    private final Map<String, Templates> faultTemplates;

    protected final Logger logger;

    /**
     * @param annotatedOperation
     *            Annotations of the operation to create
     * @param outTemplates
     *            The XSL templates used to generate the response payload
     * @param jacksonObjectMapper
     *            String to JSON converter of Jackson library.
     * @param logger
     * @throws VariableUnsupportedTypeException
     *             A variable definition contains an unsupported type.
     */
    protected FlowableOperation(final AnnotatedOperation annotatedOperation, final Templates outTemplates,
            final ObjectMapper jacksonObjectMapper, final Logger logger) throws VariableUnsupportedTypeException {
        this.wsdlOperation = annotatedOperation.getWsdlOperation();
        this.processDefinitionId = annotatedOperation.getProcessDefinitionId();
        this.userIdXPathExpr = annotatedOperation.getUserIdHolder();
        this.variables = VariableImplBuilder.build(annotatedOperation.getVariables().values(), jacksonObjectMapper,
                logger);
        this.outputTemplate = outTemplates;
        this.faultTemplates = annotatedOperation.getFaultTemplates();
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

    protected void logFlowableOperation() {
        this.logger.fine("Flowable processDefId = " + processDefinitionId);
        this.logger.fine("Flowable Action = " + this.getClass().getSimpleName());
        this.logger.fine("Flowable ActionType (TaskId) = " + this.getAction());
    }

    /**
     * Check if the current MEP has sens in the co,text of the Flowable operation
     * 
     * @param exchange
     *            The current exchange
     * @throws InvalidMEPException
     *             The MEP is not supported by this operation
     */
    protected void checkMEP(final Exchange exchange) throws InvalidMEPException {
        // By default all MEPs are accepted
    }

    @Override
    public final void execute(final Exchange exchange) {

        try {
            this.checkMEP(exchange);

            final Document incomingPayload = exchange.getInMessageContentAsDocument();

            if (this.logger.isLoggable(Level.FINE)) {
                this.logger.fine("Incoming payload = " + XMLPrettyPrinter.prettyPrint(incomingPayload));
            }

            if (this.logger.isLoggable(Level.FINE)) {
                this.logFlowableOperation();
            }

            incomingPayload.getDocumentElement().normalize();

            try {
                // Get the userId
                final String userId = this.getUserIdFromIncomingPayload(incomingPayload);

                // Get the bpmn variables
                final Map<String, Object> variableValues = this.getVariableValuesFromIncomingPayload(incomingPayload);

                // Extract process flow data
                final Map<QName, String> xslParameters = new HashMap<>();
                this.doExecute(incomingPayload, userId, variableValues, xslParameters, exchange);

                if (exchange.isInOutPattern()) {
                    try {
                        exchange.setOutMessageContent(
                                XslUtils.createXmlPayload(this.outputTemplate, xslParameters, this.logger));
                    } catch (final TransformerException e) {
                        throw new OperationProcessingException(this.wsdlOperation, e);
                    }
                }

            } catch (final OperationProcessingFault e) {
                // A fault occurs during the operation processing.

                // A fault can be returned only for MEPs 'InOut' and 'RobustInOnly'
                if (exchange.isInOutPattern() || exchange.isRobustInOnlyPattern()) {
                    final Templates faultTemplate = this.faultTemplates.get(e.getClass().getSimpleName());
                    if (faultTemplate == null) {
                        // No fault mapped --> Processing as an error
                        throw new OperationProcessingException(this.wsdlOperation, e);
                    }
                    final Fault fault = exchange.createFault();
                    try {
                        fault.setContent(XslUtils.createXmlPayload(faultTemplate, e.getXslParameters(), this.logger));
                        exchange.setFault(fault);
                    } catch (final TransformerException e1) {
                        throw new OperationProcessingException(this.wsdlOperation, e1);
                    }
                } else {
                    // pattern 'InOnly'
                    exchange.setError(e);
                }
            }
        } catch (final OperationProcessingException e) {
            // Technical error
            this.logger.log(Level.SEVERE, "Exchange " + exchange.getExchangeId() + " encountered a problem.", e);
            exchange.setError(e);
        } catch (final MessagingException e) {
            // Technical error
            this.logger.log(Level.SEVERE, "Exchange " + exchange.getExchangeId() + " encountered a problem.", e);
            exchange.setError(e);
        }
    }

    /**
     * Retrieve the user identifier from the incoming XML payload
     * 
     * @param incomingPayload
     *            The incoming XML payload
     * @return The user identifier, or {@code null} if no user id XPath expression is set
     * @throws OperationProcessingException
     *             An error occurs retrieving the user identifier
     */
    private String getUserIdFromIncomingPayload(final Document incomingPayload) throws OperationProcessingException {

        if (this.userIdXPathExpr != null) {
            final String userId;
            try {
                synchronized (this.userIdXPathExpr) {
                    userId = this.userIdXPathExpr.evaluate(incomingPayload);
                }
                if (userId == null || userId.trim().isEmpty()) {
                    throw new NoUserIdValueException(this.wsdlOperation);
                }

                if (this.logger.isLoggable(Level.FINE)) {
                    this.logger.fine("User identifier value: " + userId);
                }

                return userId;
            } catch (final XPathExpressionException e) {
                throw new OperationProcessingException(this.wsdlOperation, e);
            }
        } else {
            return null;
        }
    }

    private Map<String, Object> getVariableValuesFromIncomingPayload(final Document incomingPayload)
            throws MessagingException {
        final Map<String, Object> variableValues = new HashMap<>();
        for (final VariableImpl variable : this.variables.values()) {

            try {
                final Object variableValue = variable.extract(incomingPayload);
                variableValues.put(variable.getName(), variableValue);
            } catch (final VariableValueRequiredException e) {
                throw new MessagingException(String.format("The action '%s' of process '%s' required the variable '%s'",
                        this.getAction(), this.processDefinitionId, e.getVariableName()), e);
            } catch (final VariableException e) {
                throw new OperationProcessingException(this.wsdlOperation, e);
            }
        }
        return variableValues;
    }

    /**
     * 
     * @param incomingPayload
     *            The incoming XML payload
     * @param userId
     *            The user identifier. Can be {@code null} if no user id XPath expression is defined for the operation
     * @param processVars
     * @param outputNamedValues
     *            The output named values to generate response
     * @param exchange
     *            The exchange
     * @throws OperationProcessingException
     *             An error occurs when processing the operation
     */
    protected abstract void doExecute(final Document incomingPayload, final String userId,
            final Map<String, Object> processVars, final Map<QName, String> outputNamedValues, final Exchange exchange)
            throws OperationProcessingException;

    @Override
    public void log(final Logger logger, final Level logLevel) {
        if (logger.isLoggable(logLevel)) {
            logger.log(logLevel, "operation '" + this.getClass().getSimpleName() + "':");
            logger.log(logLevel, "\t- processDefinitionId = " + this.processDefinitionId);
            logger.log(logLevel, "\t- processInstanceId => compiled XPath expression");
            logger.log(logLevel, "\t- action = " + this.getClass().getSimpleName());
            for (final VariableImpl variable : this.variables.values()) {
                logger.log(logLevel, "\t- variable:");
                logger.log(logLevel, "\t\t- name: " + variable.getName());
                logger.log(logLevel, "\t\t- xpath: compiled XPath expression");
                logger.log(logLevel, "\t\t- type: " + variable.getType());
                if (variable instanceof VariableEnumImpl) {
                    for (final FormValue enumValue : ((VariableEnumImpl) variable).getEnumerations()) {
                        logger.log(logLevel,
                                "\t\t\t- enum value Id = " + enumValue.getId() + " - Value = " + enumValue.getName());
                    }
                } else if (variable instanceof VariableDateImpl) {
                    logger.log(logLevel, "\t\t\t- Date pattern = " + ((VariableDateImpl) variable).getDatePattern());
                }
                logger.log(logLevel, "      - required: " + variable.isRequired());
            }
        }
    }

    public String getProcessDefinitionId() {
        return this.processDefinitionId;
    }

    /**
     * @return The WSDL operation associated to this {@link FlowableOperation}
     */
    public QName getWsdlOperation() {
        return this.wsdlOperation;
    }
}
