/**
 * Copyright (c) 2017-2020 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID;
import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_CORRELATED_FLOW_STEP_ID;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.xpath.XPathExpression;
import javax.xml.xpath.XPathExpressionException;

import org.flowable.engine.HistoryService;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.runtime.Execution;
import org.flowable.engine.runtime.ExecutionQuery;
import org.ow2.petals.commons.log.FlowAttributes;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.commons.log.PetalsExecutionContext;
import org.ow2.petals.component.framework.api.message.Exchange;
import org.ow2.petals.flowable.incoming.operation.annotated.IntermediateMessageCatchEventAnnotatedOperation;
import org.ow2.petals.flowable.incoming.operation.exception.InvalidMEPException;
import org.ow2.petals.flowable.incoming.operation.exception.NoProcessInstanceIdValueException;
import org.ow2.petals.flowable.incoming.operation.exception.OperationProcessingException;
import org.ow2.petals.flowable.incoming.operation.exception.OperationProcessingFault;
import org.ow2.petals.flowable.incoming.operation.exception.ProcessInstanceEndedException;
import org.ow2.petals.flowable.incoming.operation.exception.ProcessInstanceNotFoundException;
import org.ow2.petals.flowable.incoming.operation.exception.UnexpectedMessageEventException;
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
     * The history service of the BPMN engine
     */
    private final HistoryService historyService;

    /**
     * The compiled XPath expression of the process instance identifier placeholder
     */
    private final XPathExpression proccesInstanceIdXPathExpr;

    /**
     * The message event name on which the action must be realize on the BPMN process side
     */
    private final String messageEventName;

    /**
     * The name of the activity associated to the message event receipt in the BPMN definition
     */
    private final String messageCatcherActivityId;

    /**
     * @param annotatedOperation
     *            Annotations of the operation to create
     * @param logger
     */
    public IntermediateMessageCatchEventOperation(
            final IntermediateMessageCatchEventAnnotatedOperation annotatedOperation,
            final RuntimeService runtimeService, final HistoryService historyService, final Logger logger) {
        super(annotatedOperation, null, logger);
        this.runtimeService = runtimeService;
        this.historyService = historyService;
        this.proccesInstanceIdXPathExpr = annotatedOperation.getProcessInstanceIdHolder();
        this.messageEventName = annotatedOperation.getMessageEventName();
        this.messageCatcherActivityId = annotatedOperation.getMessageCatcherActivityId();
    }

    @Override
    protected void checkMEP(final Exchange exchange) throws InvalidMEPException {
        if (!exchange.isInOnlyPattern() && !exchange.isRobustInOnlyPattern()) {
            throw new InvalidMEPException(this.wsdlOperation, exchange.getPattern().toString(), this.getAction());
        }
    }

    @Override
    protected void doExecute(final Document incomingPayload, final String bpmnUserId,
            final Map<String, Object> processVars, final Map<QName, String> outputNamedValues, final Exchange exchange,
            final boolean isFlowTracingEnabled) throws OperationProcessingException {

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
        final List<Execution> executionList = query.list();
        if ((executionList == null) || (executionList.isEmpty())) {
            throw this.investigateMissingExecution(processInstanceId);
        } else if (executionList.size() > 1) {
            throw new OperationProcessingException(this.wsdlOperation, String.format(
                    "More than one intermediate message catch event match the message '%s'.", this.messageEventName));
        }
        final String executionId = executionList.get(0).getId();

        // Set flow attributes as local variables. They will be used by Flowable event listener to generate a MONIT
        // trace
        final FlowAttributes exchangeFlowAttibutes = PetalsExecutionContext.getFlowAttributes();
        final Map<String, Object> localVariables = new HashMap<>(2);
        localVariables.put(VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID, exchangeFlowAttibutes.getFlowInstanceId());
        localVariables.put(VAR_PETALS_CORRELATED_FLOW_STEP_ID, exchangeFlowAttibutes.getFlowStepId());
        this.runtimeService.setVariablesLocal(executionId, localVariables);

        // We notify Flowable that an event was received
        this.runtimeService.setVariables(processInstanceId, processVars);
        this.runtimeService.messageEventReceivedAsync(this.messageEventName, executionId);

        // No output because only MEPs 'InOnly' and 'RobustInOnly' have sens here
    }

    /**
     * <p>
     * Investigate why no execution was found for the intermediate message catch event.
     * </p>
     * <p>
     * Several possible causes:
     * </ul>
     * <li>the process instance is finished,</li>
     * <li>no process instance with the given identifier is running,</li>
     * <li>no subscription exists for the event.</li>
     * </ul>
     * </p>
     * 
     * @param processInstanceId
     *            The process instance identifier
     * @throws OperationProcessingFault
     *             The cause why no active task was found for the process instance identifier
     * @throws OperationProcessingException
     *             No cause found.
     */
    private OperationProcessingException investigateMissingExecution(final String processInstanceId) {

        if (this.historyService.createHistoricProcessInstanceQuery().finished().processInstanceId(processInstanceId)
                .singleResult() != null) {
            // The process instance is finished, so the message event has been already received !
            // TODO: Add a unit test
            return new ProcessInstanceEndedException(this.wsdlOperation, processInstanceId, this.messageEventName);
        } else if (this.runtimeService.createProcessInstanceQuery().processInstanceId(processInstanceId)
                .singleResult() == null) {
            // No active process instance found for the process instance identifier
            // TODO: Add a unit test
            return new ProcessInstanceNotFoundException(this.wsdlOperation, processInstanceId);
        } else if (this.runtimeService.createExecutionQuery().processInstanceId(processInstanceId)
                .messageEventSubscriptionName(this.messageEventName).singleResult() == null) {
            // The message event is not expected
            return new UnexpectedMessageEventException(this.wsdlOperation, processInstanceId, this.messageEventName);
        } else {
            // This error case should not occur. If this error occurs, it is likely that an business error case is
            // missing from the above conditions
            return new OperationProcessingException(this.wsdlOperation,
                    String.format(
                            "The message '%s' is not a current intermediate message catch event for the process instance '%s'.",
                            this.messageEventName, processInstanceId));
        }
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
