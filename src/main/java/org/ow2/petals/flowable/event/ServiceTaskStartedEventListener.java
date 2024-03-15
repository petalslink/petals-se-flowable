/**
 * Copyright (c) 2015-2024 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_EXT_FLOW_TRACING_ACTIVATION_STATE;
import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_FLOW_INSTANCE_ID;
import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_FLOW_STEP_ID;

import java.util.Map;
import java.util.Optional;

import org.flowable.common.engine.api.delegate.event.FlowableEngineEventType;
import org.flowable.common.engine.api.delegate.event.FlowableEvent;
import org.flowable.common.engine.api.delegate.event.FlowableEventListener;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.delegate.event.FlowableActivityEvent;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.runtime.ProcessInstanceQuery;
import org.ow2.petals.commons.log.FlowAttributes;
import org.ow2.petals.component.framework.AbstractComponent;
import org.ow2.petals.flowable.outgoing.cxf.transport.PetalsConduit;

/**
 * The event listener fired when a service task is started to set flow attributes in a context to be retrieved by the
 * Petals transporter to set them in the Message exchange sent to the Petals service associated to the service task.
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class ServiceTaskStartedEventListener extends AbstractEventListener implements FlowableEventListener {

    private final RuntimeService runtimeService;

    public ServiceTaskStartedEventListener(final RuntimeService runtimeService, final AbstractComponent component) {
        super(FlowableEngineEventType.ACTIVITY_STARTED, component);
        this.runtimeService = runtimeService;
    }

    @Override
    protected void processEvent(final FlowableEvent event) {

        if (event instanceof FlowableActivityEvent) {
            final FlowableActivityEvent eventImpl = (FlowableActivityEvent) event;

            if ("serviceTask".equals(eventImpl.getActivityType())) {

                final ProcessInstanceQuery processInstanceQuery = this.runtimeService.createProcessInstanceQuery()
                        .processInstanceId(eventImpl.getProcessInstanceId()).includeProcessVariables();
                final ProcessInstance processInstance = processInstanceQuery.singleResult();

                final Map<String, Object> processVariables = processInstance.getProcessVariables();

                final String flowInstanceId = (String) processVariables.get(VAR_PETALS_FLOW_INSTANCE_ID);
                final String flowStepId = (String) processVariables.get(VAR_PETALS_FLOW_STEP_ID);

                final Optional<Boolean> extFlowTracingActivated;
                if (processVariables.containsKey(VAR_PETALS_EXT_FLOW_TRACING_ACTIVATION_STATE)) {
                    extFlowTracingActivated = Optional.of(Boolean
                            .valueOf((boolean) processVariables.get(VAR_PETALS_EXT_FLOW_TRACING_ACTIVATION_STATE)));
                } else {
                    extFlowTracingActivated = Optional.empty();
                }

                PetalsConduit.flowAttributes.set(new FlowAttributes(flowInstanceId, flowStepId));
                PetalsConduit.extFlowTracingActivated.set(extFlowTracingActivated);
            }
        } else {
            this.log.warning("Unexpected event implementation: " + event.getClass().getName());
        }
    }
}
