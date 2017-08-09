/**
 * Copyright (c) 2015-2017 Linagora
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
import java.util.logging.Logger;

import org.flowable.engine.TaskService;
import org.flowable.engine.common.api.delegate.event.FlowableEntityEvent;
import org.flowable.engine.common.api.delegate.event.FlowableEvent;
import org.flowable.engine.common.api.delegate.event.FlowableEventListener;
import org.flowable.engine.delegate.event.FlowableEngineEventType;
import org.flowable.engine.impl.persistence.entity.TaskEntity;
import org.flowable.engine.impl.persistence.entity.VariableInstance;
import org.ow2.petals.component.framework.logger.AbstractFlowLogData;
import org.ow2.petals.flowable.FlowableSEConstants;
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

    public UserTaskCompletedEventListener(final TaskService taskService, final Logger log) {
        super(FlowableEngineEventType.TASK_COMPLETED, taskService, log);
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

                final VariableInstance varFlowStepId = processVariables
                        .get(FlowableSEConstants.Flowable.VAR_PETALS_FLOW_STEP_ID);
                if (varFlowStepId == null) {
                    this.log.warning(String.format(MISSING_VARIABLE_PATTERN, VAR_PETALS_FLOW_STEP_ID,
                            taskEntity.getProcessInstanceId()));
                }

                final VariableInstance varCorrelatedFlowInstanceId = processVariables
                        .get(FlowableSEConstants.Flowable.VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID);
                if (varCorrelatedFlowInstanceId == null) {
                    this.log.warning(String.format(MISSING_VARIABLE_PATTERN, VAR_PETALS_CORRELATED_FLOW_INSTANCE_ID,
                            taskEntity.getProcessInstanceId()));
                }

                final VariableInstance varCorrelatedFlowStepId = processVariables
                        .get(VAR_PETALS_CORRELATED_FLOW_STEP_ID);
                if (varCorrelatedFlowStepId == null) {
                    this.log.warning(String.format(MISSING_VARIABLE_PATTERN, VAR_PETALS_CORRELATED_FLOW_STEP_ID,
                            taskEntity.getProcessInstanceId()));
                }

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
}
