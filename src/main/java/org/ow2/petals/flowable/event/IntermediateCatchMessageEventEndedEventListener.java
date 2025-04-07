/**
 * Copyright (c) 2015-2025 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID;
import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_CORRELATED_FLOW_STEP_ID;
import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_FLOW_INSTANCE_ID;
import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_FLOW_STEP_ID;

import java.util.Map;

import org.flowable.common.engine.api.delegate.event.FlowableEngineEventType;
import org.flowable.common.engine.api.delegate.event.FlowableEvent;
import org.flowable.common.engine.api.delegate.event.FlowableEventListener;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.delegate.event.FlowableMessageEvent;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.runtime.ProcessInstanceQuery;
import org.ow2.petals.component.framework.AbstractComponent;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;
import org.ow2.petals.flowable.monitoring.IntermediateCatchMessageEventFlowStepEndLogData;

/**
 * <p>
 * The event listener fired when an intermediate catch message event is ended (ie. receiving the message event).
 * </p>
 * <p>
 * This event listener is in charge of logging the associated MONIT trace. The current flow step id is available as
 * local variable in the intermediate catch message event activity. The flow instance id is available as variable at
 * process level.
 * </p>
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class IntermediateCatchMessageEventEndedEventListener extends AbstractMonitDirectLoggerEventListener
        implements FlowableEventListener {

    private final RuntimeService runtimeService;

    public IntermediateCatchMessageEventEndedEventListener(final RuntimeService runtimeService,
            final AbstractComponent component) {
        super(FlowableEngineEventType.ACTIVITY_MESSAGE_RECEIVED, component);
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

            final Map<String, Object> localVariable = this.runtimeService
                    .getVariablesLocal(messageEvent.getExecutionId());
            final String flowStepId = (String) localVariable.get(
                    VAR_PETALS_FLOW_STEP_ID);
            final String correlatedFlowInstanceId = (String) localVariable.get(VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID);
            final String correlatedFlowStepId = (String) localVariable.get(VAR_PETALS_CORRELATED_FLOW_STEP_ID);

            if (flowInstanceId != null && flowStepId != null) {
                return new IntermediateCatchMessageEventFlowStepEndLogData(flowInstanceId, flowStepId,
                        correlatedFlowInstanceId, correlatedFlowStepId);
            } else {
                this.log.warning(String.format("No MONIT end trace generated because a variable is missing: %s.",
                        flowInstanceId == null ? VAR_PETALS_FLOW_INSTANCE_ID : VAR_PETALS_FLOW_STEP_ID));
                return null;
            }
        }

        return null;

    }
}
