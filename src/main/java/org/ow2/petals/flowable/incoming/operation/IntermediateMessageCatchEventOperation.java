/**
 * Copyright (c) 2017 Linagora
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

import java.util.Map;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;

import org.flowable.engine.RuntimeService;
import org.flowable.engine.runtime.Execution;
import org.flowable.engine.runtime.ExecutionQuery;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.flowable.incoming.operation.annotated.IntermediateMessageCatchEventAnnotatedOperation;
import org.ow2.petals.flowable.incoming.operation.exception.InvalidMEPException;
import org.ow2.petals.flowable.incoming.operation.exception.NoProcessInstanceIdValueException;
import org.ow2.petals.flowable.incoming.operation.exception.OperationProcessingException;
import org.w3c.dom.Document;

/**
 * The operation to create a new instance of a process
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class IntermediateMessageCatchEventOperation extends FlowableOperation {

    /**
     * The runtime service of the BPMN engine
     */
    private final RuntimeService runtimeService;

    /**
     * The compiled XPath expression of the process instance identifier placeholder
     */
    protected final XPathExpression proccesInstanceIdXPathExpr;

    /**
     * The message event name on which the action must be realize on the BPMN process side
     */
    protected final String messageEventName;

    /**
     * @param annotatedOperation
     *            Annotations of the operation to create
     * @param logger
     */
    public IntermediateMessageCatchEventOperation(
            final IntermediateMessageCatchEventAnnotatedOperation annotatedOperation,
            final RuntimeService runtimeService, final Logger logger) {
        super(annotatedOperation, null, logger);
        this.runtimeService = runtimeService;
        this.proccesInstanceIdXPathExpr = annotatedOperation.getProcessInstanceIdHolder();
        this.messageEventName = annotatedOperation.getMessageEventName();
    }

    @Override
    protected void checkMEP(final Exchange exchange) throws InvalidMEPException {
        if (!exchange.isInOnlyPattern() && !exchange.isRobustInOnlyPattern()) {
            throw new InvalidMEPException(this.wsdlOperation, exchange.getPattern().toString(), this.getAction());
        }
    }

    @Override
    protected void doExecute(final Document incomingPayload, final String bpmnUserId,
            final Map<String, Object> variables, final Map<QName, String> outputNamedValues, final Exchange exchange)
            throws OperationProcessingException {

        // Get the process instance identifier
        final String processInstanceId;
        try {
            synchronized (this.proccesInstanceIdXPathExpr) {
                processInstanceId = this.proccesInstanceIdXPathExpr.evaluate(incomingPayload);
            }
            if (processInstanceId == null || processInstanceId.trim().isEmpty()) {
                throw new NoProcessInstanceIdValueException(this.wsdlOperation);
            }

            if (this.logger.isLoggable(Level.FINE)) {
                this.logger.fine("Process instance identifier value: " + processInstanceId);
            }
        } catch (final XPathExpressionException e) {
            throw new OperationProcessingException(this.wsdlOperation, e);
        }

        // We retrieve the Flowable execution associated to the intermediate message catcher event
        final ExecutionQuery query = this.runtimeService.createExecutionQuery().processInstanceId(processInstanceId)
                .messageEventSubscriptionName(this.messageEventName);
        final Execution execution = query.singleResult();

        // We notify Flowable that a event was received
        this.runtimeService.messageEventReceivedAsync(this.messageEventName, execution.getId());

        // No output because only MEPs 'InOnly' and 'RobustInOnly' have sens here
    }

    @Override
    public String getAction() {
        return IntermediateMessageCatchEventAnnotatedOperation.BPMN_ACTION;
    }

    @Override
    protected void logFlowableOperation() {
        super.logFlowableOperation();
        this.logger.fine("Flowable Intermediate Message Catch Event = " + this.messageEventName);
    }

}
