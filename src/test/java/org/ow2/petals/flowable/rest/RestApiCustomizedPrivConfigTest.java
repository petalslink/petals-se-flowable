/**
 * Copyright (c) 2017-2024 Linagora
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
package org.ow2.petals.flowable.rest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.File;
import java.net.ConnectException;
import java.net.URL;

import javax.xml.namespace.QName;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.ow2.petals.component.framework.junit.rule.ParameterGenerator;
import org.ow2.petals.flowable.FlowableSEConstants;

import jakarta.ws.rs.ProcessingException;
import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;

/**
 * Class for unit tests about the use of the REST API with the default privilege configuration
 * 
 * @author Victor NOEL - Linagora
 * @author Jordy CABANNES - Linagora
 * @author Christophe DENEUX - Linagora
 */
public class RestApiCustomizedPrivConfigTest extends AbstractRestTestEnvironment {

    private static final int CUSTOMIZED_PORT = 8085;

    @BeforeAll
    private static void completesComponentUnderTestConfiguration() throws Exception {
        COMPONENT_UNDER_TEST
                .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_REST_API_PORT),
                        String.valueOf(CUSTOMIZED_PORT))
                .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_REST_API_ENABLE),
                        Boolean.TRUE.toString())
                .setParameter(
                        new QName(FlowableSEConstants.NAMESPACE_COMP,
                                FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CFG_FILE),
                        // Generate identity service configuration files
                        new ParameterGenerator() {

                            @Override
                            public String generate() throws Exception {
                                final URL idmEngineConfiguratorCfg = Thread.currentThread().getContextClassLoader()
                                        .getResource("rest/idm-engine-configurator.properties");
                                assertNotNull(idmEngineConfiguratorCfg,
                                        "IDM engine configurator config file is missing !");
                                return new File(idmEngineConfiguratorCfg.toURI()).getAbsolutePath();
                            }

                        })
                .setParameter(
                        new QName(FlowableSEConstants.NAMESPACE_COMP,
                                FlowableSEConstants.ENGINE_REST_API_ACCESS_PRIVILEGE),
                        "customized-rest-access-api-privilege")
                .postInitComponentUnderTest();
    }

    /**
     * Check that we can access to the REST API with the new configuration (customized ENGINE_REST_API_PORT and
     * ENGINE_REST_API_ACCESS_PRIVILEGE).
     */
    @Test
    public void getOnApiCorrectConfig() {
        final Response response = deployments("customized-rest-api-user", "customized-user-api-rest-password",
                CUSTOMIZED_PORT).get();

        assertEquals(Status.OK.getStatusCode(), response.getStatus());
    }

    /**
     * Check that we cannot access to REST API with login/password of a user who does not belong to the customized
     * ENGINE_REST_API_ACCESS_PRIVILEGE because of incorrect login/password.
     */
    @Test
    public void getOnApiWithIncorrectLogin() {
        final Response response = deployments("fozzie", "fozzie", CUSTOMIZED_PORT).get();

        assertEquals(Status.UNAUTHORIZED.getStatusCode(), response.getStatus());
    }

    /**
     * Check that we cannot access to the REST API on the default port with login/password of a user who does not belong
     * to the customized ENGINE_REST_API_ACCESS_PRIVILEGE because the REST API is listening on another port.
     */
    @Test
    public void getOnApiDefaultPortWithIncorrectLogin() {

        final ProcessingException actualException = assertThrows(ProcessingException.class, () -> {
            deployments("fozzie", "fozzie").get();
        });
        assertInstanceOf(ConnectException.class, actualException.getCause());
    }

    /**
     * Check that we cannot access to the REST API with the default configuration (ENGINE_REST_API_PORT and
     * ENGINE_REST_API_ACCESS_PRIVILEGE) because the REST API is listening on another port.
     */
    @Test
    public void getOnApiDefaultWithCorrectLogin() {

        final ProcessingException actualException = assertThrows(ProcessingException.class, () -> {
            deployments("rest-api-user", "user-api-rest-password").get();
        });
        assertInstanceOf(ConnectException.class, actualException.getCause());
    }
}
