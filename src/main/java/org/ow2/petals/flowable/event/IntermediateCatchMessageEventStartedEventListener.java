/**
 * Copyright (c) 2015-2022 Linagora
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
package org.ow2.petals.flowable.event;

import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_FLOW_INSTANCE_ID;
import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_FLOW_STEP_ID;

import java.util.Map;
import java.util.logging.Logger;

import org.flowable.common.engine.api.delegate.event.FlowableEngineEventType;
import org.flowable.common.engine.api.delegate.event.FlowableEvent;
import org.flowable.common.engine.api.delegate.event.FlowableEventListener;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.delegate.event.FlowableMessageEvent;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.runtime.ProcessInstanceQuery;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;
import org.ow2.petals.flowable.FlowableSEConstants;
import org.ow2.petals.flowable.monitoring.IntermediateCatchMessageEventFlowStepBeginLogData;

import com.ebmwebsourcing.easycommons.uuid.SimpleUUIDGenerator;

/**
 * <p>
 * The event listener fired when an intermediate catch message event is started (ie. waiting the message event).
 * </p>
 * <p>
 * This event listener is in charge of logging the associated MONIT trace. The flow attributes of the previous step are
 * retrieved from the process variables. Flow attributes of the current step are stored as variables in the intermediate
 * catch event.
 * </p>
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class IntermediateCatchMessageEventStartedEventListener extends AbstractMonitDirectLoggerEventListener
        implements FlowableEventListener {

    private final SimpleUUIDGenerator simpleUUIDGenerator;

    private final RuntimeService runtimeService;

    public IntermediateCatchMessageEventStartedEventListener(final SimpleUUIDGenerator simpleUUIDGenerator,
            final RuntimeService runtimeService, final Logger log) {
        super(FlowableEngineEventType.ACTIVITY_MESSAGE_WAITING, log);
        this.simpleUUIDGenerator = simpleUUIDGenerator;
        this.runtimeService = runtimeService;
    }

    @Override
    protected AbstractFlowLogData createLogData(final FlowableEvent event) {

        if (event instanceof FlowableMessageEvent) {
            final FlowableMessageEvent messageEvent = (FlowableMessageEvent) event;

            final ProcessInstanceQuery processInstanceQuery = this.runtimeService.createProcessInstanceQuery()
                    .processInstanceId(messageEvent.getProcessInstanceId()).includeProcessVariables();
            final ProcessInstance processInstance = processInstanceQuery.singleResult();

            final Map<String, Object> processVariables = processInstance.getProcessVariables();
            final String flowInstanceId = (String) processVariables.get(VAR_PETALS_FLOW_INSTANCE_ID);
            final String flowPreviousStepId = (String) processVariables.get(VAR_PETALS_FLOW_STEP_ID);
            final String flowStepId = this.simpleUUIDGenerator.getNewID();

            // Flow attribute of the correlated service invocation are stored as local variables
            final Map<String, Object> localVariables = this.runtimeService
                    .getVariablesLocal(messageEvent.getExecutionId());

            // We store the flow step id as local variable of the intermediate catch message event
            this.runtimeService.setVariableLocal(messageEvent.getExecutionId(),
                    FlowableSEConstants.Flowable.VAR_PETALS_FLOW_STEP_ID, flowStepId);

            if (flowInstanceId != null && flowPreviousStepId != null) {
                return new IntermediateCatchMessageEventFlowStepBeginLogData(flowInstanceId, flowStepId,
                        flowPreviousStepId, messageEvent.getActivityId(), messageEvent.getMessageName(),
                        messageEvent.getExecutionId());
            } else {
                this.log.warning(String.format("No MONIT start trace generated because a variable is missing: %s.",
                        flowInstanceId == null ? VAR_PETALS_FLOW_INSTANCE_ID : VAR_PETALS_FLOW_STEP_ID));
                return null;
            }
        }

        return null;

    }
}
