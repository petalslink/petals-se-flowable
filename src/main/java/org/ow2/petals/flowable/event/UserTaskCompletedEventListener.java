/**
 * Copyright (c) 2015-2026 Linagora
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
import org.flowable.common.engine.api.delegate.event.FlowableEntityEvent;
import org.flowable.common.engine.api.delegate.event.FlowableEvent;
import org.flowable.common.engine.api.delegate.event.FlowableEventListener;
import org.flowable.engine.TaskService;
import org.flowable.task.service.impl.persistence.entity.TaskEntity;
import org.flowable.variable.api.persistence.entity.VariableInstance;
import org.ow2.petals.component.framework.AbstractComponent;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;
import org.ow2.petals.flowable.monitoring.UserTaskFlowStepEndLogData;

/**
 * <p>
 * The event listener fired when a user task is completed.
 * </p>
 * <p>
 * This event listener is in charge of logging the associated MONIT trace. The flow attributes are retrieved from the
 * process variables and task variables.
 * </p>
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class UserTaskCompletedEventListener extends AbstractTaskEventListener implements FlowableEventListener {

    public UserTaskCompletedEventListener(final TaskService taskService, final AbstractComponent component) {
        super(FlowableEngineEventType.TASK_COMPLETED, taskService, component);
    }

    @Override
    protected AbstractFlowLogData createLogData(final FlowableEvent event) {

        if (event instanceof FlowableEntityEvent) {
            final FlowableEntityEvent entityEvent = (FlowableEntityEvent) event;
            final Object entity = entityEvent.getEntity();
            if (entity instanceof TaskEntity) {
                final TaskEntity taskEntity = (TaskEntity) entity;

                final VariableInstance varFlowInstanceId = this.getVariable(taskEntity, VAR_PETALS_FLOW_INSTANCE_ID);
                final VariableInstance varFlowStepId = this.getVariable(taskEntity, VAR_PETALS_FLOW_STEP_ID);
                final VariableInstance varCorrelatedFlowInstanceId = this.getVariable(taskEntity,
                        VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID);
                final VariableInstance varCorrelatedFlowStepId = this.getVariable(taskEntity,
                        VAR_PETALS_CORRELATED_FLOW_STEP_ID);

                if (varFlowInstanceId != null && varFlowStepId != null && varCorrelatedFlowInstanceId != null
                        && varCorrelatedFlowStepId != null) {
                    return new UserTaskFlowStepEndLogData((String) varFlowInstanceId.getValue(),
                            (String) varFlowStepId.getValue(), (String) varCorrelatedFlowInstanceId.getValue(),
                            (String) varCorrelatedFlowStepId.getValue());
                } else {
                    this.log.warning("No MONIT end trace generated because a process instance variable is missing.");
                    return null;
                }

            } else {
                this.log.warning("Unexpected event entity implementation: " + entity.getClass().getName());
                return null;
            }
        } else {
            this.log.warning("Unexpected event implementation: " + event.getClass().getName());
            return null;
        }

    }

    private VariableInstance getVariable(final TaskEntity taskEntity, final String variableName) {

        final Map<String, VariableInstance> processVariables = taskEntity.getVariableInstances();

        final VariableInstance varFlowStepId = processVariables.get(variableName);
        if (varFlowStepId == null) {
            this.log.warning(String.format(MISSING_VARIABLE_PATTERN, variableName, taskEntity.getProcessInstanceId()));
        }

        return varFlowStepId;
    }
}
