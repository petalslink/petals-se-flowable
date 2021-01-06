/**
 * Copyright (c) 2017-2021 Linagora
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
package org.ow2.petals.flowable.identity.ldap;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.Properties;

import org.flowable.engine.IdentityService;
import org.flowable.idm.api.Group;
import org.flowable.idm.api.User;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;
import org.ow2.petals.flowable.identity.exception.IdentityServiceInitException;
import org.ow2.petals.flowable.junit.FlowableClient;
import org.zapodot.junit.ldap.EmbeddedLdapRule;
import org.zapodot.junit.ldap.EmbeddedLdapRuleBuilder;

public class LdapIdentityServiceImplTest {

    public static final String DOMAIN_DSN = "dc=petals,dc=org";

    @Rule
    public EmbeddedLdapRule embeddedLdapRule = EmbeddedLdapRuleBuilder.newInstance().usingDomainDsn(DOMAIN_DSN)
            .usingBindDSN("cn=Petals ESB").usingBindCredentials("abcdefg")
            .importingLdifs("users-import.ldif").build();

    @Rule
    public final TemporaryFolder tempFolder = new TemporaryFolder();

    /**
     * Check the loading of a given configuration.
     */
    @Test
    public void givenConfiguration() throws Exception {

        // We generate a configuration from the default one
        final InputStream defaultCfgInputStream = this.getClass()
                .getResourceAsStream("/ldap-idm-configurator.properties");
        assertNotNull("LDAP IDM configuration config file not found", defaultCfgInputStream);
        final Properties ldapConfig = new Properties();
        try {
            ldapConfig.load(defaultCfgInputStream);
        } catch (final IOException e) {
            throw new IdentityServiceInitException(e);
        }

        ldapConfig.setProperty(LdapIdmEngineConfigurator.PROP_PORT,
                Integer.toString(this.embeddedLdapRule.embeddedServerPort()));
        ldapConfig.setProperty(LdapIdmEngineConfigurator.PROP_USER, "cn=Petals ESB");
        ldapConfig.setProperty(LdapIdmEngineConfigurator.PROP_PASSWORD, "abcdefg");

        final File ldapConfigFile = this.tempFolder.newFile("ldap-idm-configurator.properties");
        final OutputStream osConfigFile = new FileOutputStream(ldapConfigFile);
        try {
            ldapConfig.store(osConfigFile, "");
        } finally {
            osConfigFile.close();
        }

        final FlowableClient flowableClient = new FlowableClient(new LdapIdmEngineConfigurator(), ldapConfigFile);
        flowableClient.create();
        try {
            final IdentityService identityService = flowableClient.getIdentityService();

            final User kermit = identityService.createUserQuery().userId("kermit").singleResult();
            assertNotNull(kermit);
            assertEquals("Flowable", kermit.getLastName());
            assertEquals("Kermit", kermit.getFirstName());
            assertEquals("kermit@petals.org", kermit.getEmail());

            assertTrue(identityService.checkPassword("kermit", "abcdefg"));
            assertFalse(identityService.checkPassword("fozzie", "invalid-pwd"));

            final List<Group> fozzieGroups = identityService.createGroupQuery()
                    .groupMember("fozzie").list();
            assertEquals(2, fozzieGroups.size());

        } finally {
            flowableClient.delete();
        }
    }

}
