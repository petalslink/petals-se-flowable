/**
 * Copyright (c) 2015-2023 Linagora
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

import org.flowable.common.engine.api.delegate.event.FlowableEngineEventType;
import org.flowable.common.engine.api.delegate.event.FlowableEntityEvent;
import org.flowable.common.engine.api.delegate.event.FlowableEvent;
import org.flowable.common.engine.api.delegate.event.FlowableEventListener;
import org.flowable.engine.TaskService;
import org.flowable.task.service.impl.persistence.entity.TaskEntity;
import org.flowable.variable.api.persistence.entity.VariableInstance;
import org.ow2.petals.component.framework.AbstractComponent;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;
import org.ow2.petals.flowable.monitoring.UserTaskFlowStepBeginLogData;

import com.ebmwebsourcing.easycommons.uuid.SimpleUUIDGenerator;

/**
 * <p>
 * The event listener fired when a user task is started.
 * </p>
 * <p>
 * This event listener is in charge of logging the associated MONIT trace. The flow attributes are retrieved from the
 * process variables and task variables.
 * </p>
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class UserTaskStartedEventListener extends AbstractTaskEventListener implements FlowableEventListener {

    private final SimpleUUIDGenerator simpleUUIDGenerator;

    public UserTaskStartedEventListener(final SimpleUUIDGenerator simpleUUIDGenerator, final TaskService taskService,
            final AbstractComponent component) {
        super(FlowableEngineEventType.TASK_CREATED, taskService, component);
        this.simpleUUIDGenerator = simpleUUIDGenerator;
    }

    @Override
    protected AbstractFlowLogData createLogData(final FlowableEvent event) {

        if (event instanceof FlowableEntityEvent) {
            final FlowableEntityEvent entityEvent = (FlowableEntityEvent) event;
            final Object entity = entityEvent.getEntity();
            if (entity instanceof TaskEntity) {
                final TaskEntity taskEntity = (TaskEntity) entity;

                final Map<String, VariableInstance> processVariables = taskEntity.getVariableInstances();

                final VariableInstance varFlowInstanceId = processVariables.get(VAR_PETALS_FLOW_INSTANCE_ID);
                if (varFlowInstanceId == null) {
                    this.log.warning(String.format(MISSING_VARIABLE_PATTERN, VAR_PETALS_FLOW_INSTANCE_ID,
                            taskEntity.getProcessInstanceId()));
                }

                // TODO: Review the value of the flow previous step identifier to manage correctly process with branches
                final VariableInstance varFlowPreviousStepId = processVariables.get(VAR_PETALS_FLOW_STEP_ID);
                if (varFlowPreviousStepId == null) {
                    this.log.warning(String.format(MISSING_VARIABLE_PATTERN, VAR_PETALS_FLOW_STEP_ID,
                            taskEntity.getProcessInstanceId()));
                }

                final String flowStepId = this.simpleUUIDGenerator.getNewID();

                // We store the flow step id as task local variable
                this.taskService.setVariableLocal(taskEntity.getId(), VAR_PETALS_FLOW_STEP_ID, flowStepId);

                if (varFlowInstanceId != null && varFlowPreviousStepId != null) {
                    return new UserTaskFlowStepBeginLogData((String) varFlowInstanceId.getValue(), flowStepId,
                            (String) varFlowPreviousStepId.getValue(), taskEntity.getTaskDefinitionKey(),
                            taskEntity.getId());
                } else {
                    this.log.warning("No MONIT start trace generated because a variable is missing.");
                    return null;
                }
            }
        }

        return null;

    }
}
