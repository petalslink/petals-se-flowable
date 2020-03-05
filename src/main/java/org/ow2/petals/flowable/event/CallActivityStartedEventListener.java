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
package org.ow2.petals.flowable.event;

import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_FLOW_INSTANCE_ID;
import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_FLOW_STEP_ID;
import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.VAR_PETALS_PLACEHOLDERS;

import java.util.Map;
import java.util.logging.Logger;

import org.flowable.engine.RuntimeService;
import org.flowable.engine.common.api.delegate.event.FlowableEngineEventType;
import org.flowable.engine.common.api.delegate.event.FlowableEvent;
import org.flowable.engine.common.api.delegate.event.FlowableEventListener;
import org.flowable.engine.delegate.event.impl.FlowableProcessStartedEventImpl;
import org.flowable.engine.impl.persistence.entity.ExecutionEntity;
import org.flowable.engine.runtime.ProcessInstance;
import org.flowable.engine.runtime.ProcessInstanceQuery;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;
import org.ow2.petals.flowable.CallActivityForceAsyncParseHandler;
import org.ow2.petals.flowable.monitoring.CallActivityFlowStepBeginLogData;

import com.ebmwebsourcing.easycommons.uuid.SimpleUUIDGenerator;

/**
 * The event listener fired when a process instance is started to log the MONIT trace.
 * 
 * Note: To be able to retrieve variables of the parent process, the call activity must be executed asynchronously. See
 * {@link CallActivityForceAsyncParseHandler}.
 * 
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class CallActivityStartedEventListener extends AbstractMonitDirectLoggerEventListener
        implements FlowableEventListener {

    private final RuntimeService runtimeService;

    private final SimpleUUIDGenerator simpleUUIDGenerator;

    public CallActivityStartedEventListener(final RuntimeService runtimeService,
            final SimpleUUIDGenerator simpleUUIDGenerator, final Logger log) {
        super(FlowableEngineEventType.PROCESS_STARTED, log);
        this.runtimeService = runtimeService;
        this.simpleUUIDGenerator = simpleUUIDGenerator;
    }

    @Override
    protected AbstractFlowLogData createLogData(final FlowableEvent event) {

        if (event instanceof FlowableProcessStartedEventImpl) {
            final FlowableProcessStartedEventImpl eventImpl = (FlowableProcessStartedEventImpl) event;

            if (eventImpl.getNestedProcessDefinitionId() != null) {

                final String callActivityInstanceId = eventImpl.getProcessInstanceId();
                final String parentInstanceId = eventImpl.getNestedProcessInstanceId();
                final ExecutionEntity callActivityEntity = (ExecutionEntity) eventImpl.getEntity();

                this.log.fine(String.format("The call activity '%s' (instance id '%s') is started from instance '%s'.",
                        callActivityEntity.getProcessDefinitionKey(), callActivityInstanceId, parentInstanceId));

                // To be able to retrieve variables of the parent process, the call activity must be executed
                // asynchronously. See note in this class documentation.
                final ProcessInstanceQuery processInstanceQuery = this.runtimeService.createProcessInstanceQuery()
                        .processInstanceId(parentInstanceId).includeProcessVariables();
                final ProcessInstance processInstance = processInstanceQuery.singleResult();

                final Map<String, Object> processVariables = processInstance.getProcessVariables();

                final String flowInstanceId = (String) processVariables.get(VAR_PETALS_FLOW_INSTANCE_ID);

                final String previousFlowStepId = (String) processVariables.get(VAR_PETALS_FLOW_STEP_ID);
                if (previousFlowStepId == null) {
                    this.log.warning(
                            String.format(MISSING_VARIABLE_PATTERN, VAR_PETALS_FLOW_STEP_ID, parentInstanceId));
                }

                final String flowStepId = this.simpleUUIDGenerator.getNewID();

                // final Map callACtivityVariables = eventImpl.getVariables();
                // callACtivityVariables.put(FlowableSEConstants.Flowable.VAR_PETALS_FLOW_INSTANCE_ID, flowInstanceId);
                // callACtivityVariables.put(FlowableSEConstants.Flowable.VAR_PETALS_FLOW_STEP_ID, flowStepId);

                this.runtimeService.setVariable(eventImpl.getExecutionId(),
                        VAR_PETALS_FLOW_INSTANCE_ID, flowInstanceId);
                this.runtimeService.setVariable(eventImpl.getExecutionId(),
                        VAR_PETALS_FLOW_STEP_ID, flowStepId);

                // Placeholders must be copied in the call activity scope
                this.runtimeService.setVariable(eventImpl.getExecutionId(), VAR_PETALS_PLACEHOLDERS,
                        processVariables.get(VAR_PETALS_PLACEHOLDERS));

                final ExecutionEntity parentEntity = callActivityEntity.getParent();

                return new CallActivityFlowStepBeginLogData(flowInstanceId, flowStepId, previousFlowStepId,
                        parentEntity.getProcessDefinitionKey(), parentInstanceId,
                        callActivityEntity.getProcessDefinitionKey(), callActivityInstanceId);
            } else {
                // It's a nominal process instance start. It is processed by ProcessInstanceStartedEventListener
                return null;
            }

        } else {
            this.log.warning("Unexpected event implementation: " + event.getClass().getName());
            return null;
        }
    }
}
